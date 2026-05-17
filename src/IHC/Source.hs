-- | Immutable source buffer + cheap byte-offset cursors.
--
-- A @Source@ is the whole file's bytes plus a filename. All position
-- tracking is done with @Int@ byte offsets; line/column are recovered lazily
-- when we need to report a diagnostic.
--
-- Line-start offsets are cached at construction time (one linear scan) so
-- that 'offsetToPos' can use binary search — O(log n) per diagnostic call.
module IHC.Source
    ( Source(..)
    , Pos
    , Span
    , mkSource
    , readSourceFile
    , atEnd
    , peekByte
    , takeByte
    , sliceBytes
    , lineCol
    , offsetToPos
    , withBytes
    , ScanCacheBox
    , readScanCache
    , writeScanCache
      -- Exposed only so 'IHC.MemDebug' can size it for the
      -- @IHC_MEM_DEBUG@ probe.  NOT a per-run reset target: this
      -- registry is the content-addressed cross-run scan-cache
      -- amortization (see 'mkFreshScanCache') and must survive
      -- 'IHC.Scheduler.resetPerRunGlobals'.
    , globalScanCacheRegistry
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Dynamic (Dynamic)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import Data.Word (Word8)
import System.IO.Unsafe (unsafePerformIO)

type Pos = Int           -- ^ byte offset into the source
type Span = (Pos, Pos)   -- ^ half-open [start, end)

-- | Memoisation slot for scan results. The 'IHC.Scan' module stores
-- its uncached scan output here on the first call so that repeat
-- callers (Scheduler.hs invokes each scanFooDecls 5–10 times per
-- loaded module) pay no repeat lex cost.  Keyed by an arbitrary
-- string tag chosen by the scan function; the value is a 'Dynamic'
-- so we don't have to enumerate every concrete result type in this
-- module.
--
-- Content-addressable: two 'Source' values built from the same byte
-- content share one 'IORef' (see 'mkFreshScanCache').  Sound because
-- scan results are pure functions of the bytes; necessary because
-- the Scheduler builds many 'Source' values over the same files.
newtype ScanCacheBox = ScanCacheBox (IORef (Map String Dynamic))

readScanCache :: ScanCacheBox -> String -> IO (Maybe Dynamic)
readScanCache (ScanCacheBox ref) tag = Map.lookup tag <$> readIORef ref
{-# INLINE readScanCache #-}

writeScanCache :: ScanCacheBox -> String -> Dynamic -> IO ()
writeScanCache (ScanCacheBox ref) tag v =
    atomicModifyIORef' ref (\m -> (Map.insert tag v m, ()))
{-# INLINE writeScanCache #-}

data Source = Source
    { srcName      :: !FilePath
    , srcBytes     :: !ByteString
    -- | Byte offsets of the first byte of each line, 0-indexed.
    -- @srcLineStarts !! 0 == 0@ always.  Computed once in 'mkSource'.
    , srcLineStarts :: ![Int]
    -- | Memoisation slot for scan results.  Content-addressable: two
    -- 'mkSource' calls with the same bytes share one 'IORef'.  See
    -- 'mkFreshScanCache'.
    , srcScanCache :: ScanCacheBox
    }

-- | Build a 'Source' from a file path and its raw bytes.
-- One linear scan over @bs@ builds the line-start table; all subsequent
-- 'offsetToPos' calls pay only O(log n).
--
-- Attaches a fresh 'ScanCacheBox' so 'IHC.Scan.memoiseScan' can cache
-- per-Source.  Treat this as a pure constructor — the cache is just an
-- under-the-hood detail.
mkSource :: FilePath -> ByteString -> Source
mkSource name bs = Source name bs (buildLineStarts bs) (mkFreshScanCache name bs)

-- | Allocate a scan cache for a 'Source'.
--
-- This is a CONTENT-ADDRESSABLE cache: two 'mkSource' calls with the
-- same byte content share the same underlying 'IORef'.  Sound because
-- all cached values are pure functions of the bytes (the @tag@ is the
-- only other input, scoped inside the inner Map), and necessary
-- because the Scheduler builds many 'Source' values over the same
-- files (entry-module probe, header parse, discovery pass, REPL load,
-- etc.) — without sharing, each scan would pay the full lex cost per
-- Source instead of once per file.
--
-- The previous design tried to give every 'Source' its own 'IORef'
-- via @unsafePerformIO (newIORef Map.empty)@ inside the function
-- body, with a @(name, bs)@ "cookie" of strict bang patterns intended
-- to defeat GHC's CSE.  That doesn't actually work: the body has no
-- free variables, so GHC is entitled to float it out as a top-level
-- CAF and every call shared one 'IORef'.  That made
-- 'scanAllTopLevelNames' (and friends) return the FIRST source's
-- result for every subsequent call — a correctness bug masquerading
-- as a speedup.  We side-step both problems by keying off the bytes
-- themselves.
mkFreshScanCache :: FilePath -> ByteString -> ScanCacheBox
mkFreshScanCache _name bs = unsafePerformIO $ do
    m <- readIORef globalScanCacheRegistry
    case Map.lookup bs m of
        Just ref -> pure (ScanCacheBox ref)
        Nothing -> do
            ref <- newIORef Map.empty
            atomicModifyIORef' globalScanCacheRegistry
                (\m' -> case Map.lookup bs m' of
                    Just existing -> (m', ScanCacheBox existing)
                    Nothing       -> (Map.insert bs ref m', ScanCacheBox ref))
{-# NOINLINE mkFreshScanCache #-}

{-# NOINLINE globalScanCacheRegistry #-}
globalScanCacheRegistry :: IORef (Map ByteString (IORef (Map String Dynamic)))
globalScanCacheRegistry = unsafePerformIO (newIORef Map.empty)

-- | Scan @bs@ once and return the byte offset of the first byte on each
-- line.  Line 1 always starts at offset 0.
buildLineStarts :: ByteString -> [Int]
buildLineStarts bs = 0 : go 0
  where
    n = BS.length bs
    go i
      | i >= n               = []
      | BS.index bs i == 0x0A = let j = i + 1 in j : go j
      | otherwise             = go (i + 1)

readSourceFile :: FilePath -> IO Source
readSourceFile path = mkSource path <$> BS.readFile path

-- | Replace the byte content of a 'Source', recomputing the line-start table.
-- Use this instead of record-update on 'srcBytes' so 'offsetToPos' stays valid.
withBytes :: Source -> ByteString -> Source
withBytes s bs = mkSource (srcName s) bs

atEnd :: Source -> Pos -> Bool
atEnd s p = p >= BS.length (srcBytes s)

peekByte :: Source -> Pos -> Maybe Word8
peekByte s p
    | p >= BS.length (srcBytes s) = Nothing
    | otherwise                   = Just (BS.index (srcBytes s) p)

takeByte :: Source -> Pos -> Maybe (Word8, Pos)
takeByte s p = case peekByte s p of
    Nothing -> Nothing
    Just b  -> Just (b, p + 1)

sliceBytes :: Source -> Span -> ByteString
sliceBytes s (a, b) = BS.take (b - a) (BS.drop a (srcBytes s))

-- | Recover 1-based (line, col) from a byte offset. Linear scan — only called
-- for diagnostics, never in the hot path.
-- Prefer 'offsetToPos' when the source was built with 'mkSource'.
lineCol :: Source -> Pos -> (Int, Int)
lineCol s pos = go 0 1 1
  where
    bs = srcBytes s
    n  = min pos (BS.length bs)
    go i line col
        | i >= n    = (line, col)
        | otherwise = case BS.index bs i of
            0x0A -> go (i + 1) (line + 1) 1
            _    -> go (i + 1) line (col + 1)

-- | O(log n) version of 'lineCol' using the cached line-start table.
-- Returns 1-based (line, col).  Falls back gracefully for out-of-range offsets.
offsetToPos :: Source -> Pos -> (Int, Int)
offsetToPos s pos = (line, col)
  where
    starts = srcLineStarts s
    -- Binary search: find the largest index i such that starts[i] <= pos.
    -- We iterate over the list; for diagnostic use O(log n) is not
    -- critical but we do a simple counted binary search via 'bsearch'.
    line   = bsearch starts pos 1 (length starts)
    start  = starts !! (line - 1)
    col    = pos - start + 1

-- | Binary search on a sorted list represented as @[Int]@.
-- Returns the 1-based index of the largest element <= target,
-- or 1 if all elements are greater.
bsearch :: [Int] -> Int -> Int -> Int -> Int
bsearch xs target lo hi
    | lo >= hi  = lo
    | otherwise =
        let mid     = (lo + hi + 1) `div` 2
            midVal  = xs !! (mid - 1)
        in if midVal <= target
               then bsearch xs target mid hi
               else bsearch xs target lo (mid - 1)
