-- | Two-phase demand-driven scheduler.
--
-- Phase A (discover): starting from a root name, recursively parse each
-- reachable binding to its 'Item' list, memoizing by name. Follows the
-- plan's demand-driven rule — bindings not transitively reachable from
-- the root are never parsed.
--
-- Phase B (layout + emit): we know every reachable binding and its
-- byte size (from 'bindingBytes'). Assign each an offset in the code
-- buffer, then emit them one at a time with all cross-call addresses
-- already resolved. Self- and mutual-recursion fall out for free
-- because every address is known before we write the first byte.
--
-- W^X: 'compileRoot' brackets the whole emit phase in writable mode
-- and flips to executable at the end. Recursive compilation happens
-- entirely in Haskell land (parsing), so there's no nested toggle.
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

import IHC.CodeBuffer
import IHC.Emit (emitBinding)
import IHC.IR
import IHC.Jit (jitWritable, jitExecutable, jitFlush)
import qualified IHC.Parser as Parser
import IHC.Scan
import IHC.Source

data Scheduler = Scheduler
    { schedSrc     :: !Source
    , schedBuf     :: !CodeBuffer
    , schedKnown   :: !KnownSymbols
    , schedBodies  :: !(IORef (Map ByteString [Item]))
    }

newScheduler :: Source -> IO Scheduler
newScheduler src = do
    buf    <- newCodeBuffer (64 * 1024)
    known  <- emptyKnownSymbols
    bodies <- newIORef Map.empty
    pure (Scheduler src buf known bodies)

freeScheduler :: Scheduler -> IO ()
freeScheduler s = freeCodeBuffer (schedBuf s)

-- | Compile the given root (typically @"main"@), returning its entry
-- pointer. The JIT buffer is executable and I-cache-flushed on return;
-- the caller may call the entry directly.
compileRoot :: Scheduler -> ByteString -> IO (Ptr ())
compileRoot s root = do
    -- Phase A: discover + parse every binding reachable from root.
    discover s root

    -- Phase B.1: layout — assign each binding an entry offset.
    -- We process bindings in ascending key order and emit them in the
    -- *same* order so offsets match bump-pointer positions.
    bodies <- readIORef (schedBodies s)
    let sortedBodies       = Map.toAscList bodies
        (addrs, totalSize) = layout (cbBase (schedBuf s)) sortedBodies

    -- Phase B.2: emit each binding at its assigned address.
    jitWritable
    mapM_ (\(_, items) -> emitBinding (schedBuf s) addrs items) sortedBodies

    -- Flip to executable + flush I-cache over all emitted bytes.
    jitExecutable
    jitFlush (cbBase (schedBuf s)) totalSize

    case Map.lookup root addrs of
        Just p  -> pure p
        Nothing -> error ("IHC.Scheduler.compileRoot: missing root `"
                          <> BC.unpack root <> "` after layout")

-- | Phase A — parse @name@ and every binding it calls into an Item map.
discover :: Scheduler -> ByteString -> IO ()
discover s name = do
    bodies <- readIORef (schedBodies s)
    if Map.member name bodies
        then pure ()     -- already discovered; break cycles
        else do
            mspan <- findOrResolveSpan s name
            case mspan of
                Nothing -> error ("IHC.Scheduler.discover: no binding `"
                                  <> BC.unpack name <> "`")
                Just span_ -> do
                    items <- Parser.parseBodyItems (schedSrc s) span_
                    modifyIORef' (schedBodies s) (Map.insert name items)
                    mapM_ (discover s) (callees items)

findOrResolveSpan :: Scheduler -> ByteString -> IO (Maybe Span)
findOrResolveSpan s name = do
    existing <- lookupSymbol (schedKnown s) name
    case existing of
        Just (SpanOnly sp) -> pure (Just sp)
        Just (Compiled _)  -> pure Nothing
        Nothing            -> findBinding (schedSrc s) (schedKnown s) name

callees :: [Item] -> [ByteString]
callees = concatMap toCall
  where
    toCall (ICall n) = [n]
    toCall _         = []

-- | Phase B.1 — walk the sorted binding list left-to-right, assigning
-- each binding an entry address from a running offset. Returns the
-- address map plus the total bytes used.
layout :: Ptr () -> [(ByteString, [Item])] -> (Map ByteString (Ptr ()), Int)
layout base = go Map.empty 0
  where
    go !acc !off []                = (acc, off)
    go !acc !off ((name, items):xs) =
        let entry = base `plusPtr` off
            sz    = bindingBytes items
        in go (Map.insert name entry acc) (off + sz) xs
