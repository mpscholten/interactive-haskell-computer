-- | Two-phase demand-driven scheduler.
--
-- Phase A (discover): starting from a root name, recursively parse each
-- reachable binding to a 'Binding' (params + items), memoizing by name.
-- Bindings not transitively reachable from the root are never parsed.
--
-- Phase B (layout + emit): we know every reachable binding and its
-- byte size. Assign each an offset, then emit bindings one at a time
-- with all cross-call addresses resolved. Self- and mutual-recursion
-- fall out for free.
module IHC.Scheduler
    ( Scheduler
    , newScheduler
    , freeScheduler
    , compileRoot
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Foreign.Ptr (Ptr, plusPtr)
import qualified Foreign.Ptr

import IHC.CodeBuffer
import IHC.Emit (emitBinding)
import IHC.IR
import IHC.Jit (jitWritable, jitExecutable, jitFlush)
import qualified IHC.Parser as Parser
import IHC.Scan
import IHC.Source

import System.Environment (lookupEnv)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Char8 as BC
import Foreign.Marshal.Array (peekArray)
import Data.Word (Word8)
import Numeric (showHex)

data Scheduler = Scheduler
    { schedSrc     :: !Source
    , schedBuf     :: !CodeBuffer
    , schedKnown   :: !KnownSymbols
    , schedBodies  :: !(IORef (Map ByteString Binding))
    }

newScheduler :: Source -> IO Scheduler
newScheduler src = do
    buf    <- newCodeBuffer (64 * 1024)
    known  <- emptyKnownSymbols
    bodies <- newIORef Map.empty
    pure (Scheduler src buf known bodies)

freeScheduler :: Scheduler -> IO ()
freeScheduler s = freeCodeBuffer (schedBuf s)

-- | Compile the given root, returning its entry pointer. On return,
-- the JIT buffer is executable and I-cache-flushed.
compileRoot :: Scheduler -> ByteString -> IO (Ptr ())
compileRoot s root = do
    discover s root

    bodies <- readIORef (schedBodies s)
    let sortedBodies       = Map.toAscList bodies
        (addrs, totalSize) = layout (cbBase (schedBuf s)) sortedBodies

    jitWritable
    mapM_ (\(_, b) -> emitBinding (schedBuf s) addrs b) sortedBodies

    -- Optional dump of the emitted bytes for debugging.
    dumpEnv <- lookupEnv "IHC_DUMP_JIT"
    case dumpEnv of
        Just _ -> dumpBuffer (cbBase (schedBuf s)) totalSize addrs
        Nothing -> pure ()

    jitExecutable
    jitFlush (cbBase (schedBuf s)) totalSize

    case Map.lookup root addrs of
        Just p  -> pure p
        Nothing -> error ("IHC.Scheduler.compileRoot: missing root `"
                          <> BC.unpack root <> "` after layout")

-- | Phase A — parse @name@ and every binding it calls.
discover :: Scheduler -> ByteString -> IO ()
discover s name = do
    bodies <- readIORef (schedBodies s)
    if Map.member name bodies
        then pure ()
        else do
            mLhs <- findOrResolveLhs s name
            case mLhs of
                Nothing -> error ("IHC.Scheduler.discover: no binding `"
                                  <> BC.unpack name <> "`")
                Just lhs -> do
                    items <- Parser.parseBodyItems
                                (schedSrc s)
                                (lhsParams lhs)
                                (arityResolver s)
                                (lhsBody lhs)
                    let b = Binding (lhsParams lhs) items
                    modifyIORef' (schedBodies s) (Map.insert name b)
                    mapM_ (discover s) (callees items)

-- | Arity resolver used by the parser: look up (or lazily discover)
-- a referenced binding's LHS and report its param count.
arityResolver :: Scheduler -> ByteString -> IO Int
arityResolver s name = do
    mLhs <- findOrResolveLhs s name
    case mLhs of
        Just lhs -> pure (length (lhsParams lhs))
        Nothing  -> error ("IHC.Scheduler.arityResolver: unknown binding `"
                           <> BC.unpack name <> "`")

findOrResolveLhs :: Scheduler -> ByteString -> IO (Maybe BindingLhs)
findOrResolveLhs s name = do
    existing <- lookupSymbol (schedKnown s) name
    case existing of
        Just (SpanOnly lhs) -> pure (Just lhs)
        Just (Compiled _)   -> pure Nothing
        Nothing             -> findBinding (schedSrc s) (schedKnown s) name

callees :: [Item] -> [ByteString]
callees = concatMap toCall
  where
    toCall (ICall n _)  = [n]
    toCall (IIfThenElse c t e) =
        concatMap toCall c ++ concatMap toCall t ++ concatMap toCall e
    toCall _            = []

-- | Dump the code buffer as hex groups of 4 bytes per line. Prints
-- "name:" at entries that match an address.
dumpBuffer :: Ptr () -> Int -> Map ByteString (Ptr ()) -> IO ()
dumpBuffer base nBytes addrs = do
    -- Build a reverse map from address to name for labelling.
    let nameOf p = fst <$> Map.lookupMin (Map.filter (== p) addrs)
    bs <- peekArray nBytes (castPtr' base)
    hPutStrLn stderr ("=== JIT dump (" <> show nBytes <> " bytes) ===")
    let groups = chunk4 bs
    mapM_ (\(off, ws) -> do
        case nameOf (base `plusPtr` off) of
            Just nm -> hPutStrLn stderr ("\n" <> BC.unpack nm <> ":")
            Nothing -> pure ()
        hPutStrLn stderr (pad (showHex off "") <> ":  " <> fmtInsn ws))
        (zip [0, 4 ..] groups)
  where
    chunk4 [] = []
    chunk4 xs = let (a, b) = splitAt 4 xs in a : chunk4 b
    fmtInsn [a,b,c,d] = pad (showHex (unpackLE a b c d) "")
    fmtInsn xs = concatMap (\x -> pad2 (showHex x "") <> " ") xs
    pad s  = replicate (8 - length s) '0' <> s
    pad2 s = replicate (2 - length s) '0' <> s
    unpackLE a b c d =
        fromIntegral a
        + fromIntegral b * 0x100
        + fromIntegral c * 0x10000
        + (fromIntegral d * 0x1000000 :: Word)
    castPtr' :: Ptr () -> Ptr Word8
    castPtr' = Foreign.Ptr.castPtr

-- | Phase B.1 — walk the sorted binding list, assigning each an entry
-- address from a running offset.
layout :: Ptr () -> [(ByteString, Binding)] -> (Map ByteString (Ptr ()), Int)
layout base = go Map.empty 0
  where
    go !acc !off []                = (acc, off)
    go !acc !off ((name, b):xs) =
        let entry = base `plusPtr` off
            sz    = bindingBytes b
        in go (Map.insert name entry acc) (off + sz) xs
