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
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Dynamic (Dynamic)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef)
import Data.Word (Word8)

type Pos = Int           -- ^ byte offset into the source
type Span = (Pos, Pos)   -- ^ half-open [start, end)

-- | Per-'Source' memoisation slot for scan results. The 'IHC.Scan' module
-- stores its uncached scan output here on the first call so that repeat
-- callers (Scheduler.hs invokes each scanFooDecls 5–10 times per loaded
-- module) pay no repeat lex cost. Keyed by an arbitrary string tag chosen
-- by the scan function; the value is a 'Dynamic' so we don't have to
-- enumerate every concrete result type in this module.
--
-- Per-'Source' (not global): two distinct 'Source' values, even if they
-- happen to share a 'srcName' (e.g. multiple @<repl>@ inputs), get
-- distinct 'IORef's, so a cache hit on one never leaks into the other.
newtype ScanCacheBox = ScanCacheBox (IORef (Map String Dynamic))

-- | Look up a previously-cached scan result for this source.
readScanCache :: ScanCacheBox -> String -> IO (Maybe Dynamic)
readScanCache (ScanCacheBox ref) tag = Map.lookup tag <$> readIORef ref
{-# INLINE readScanCache #-}

-- | Insert/overwrite a scan result. 'atomicModifyIORef'' so concurrent
-- scans of the same source don't lose entries; double-compute on a race
-- is harmless (scan results are pure in the source bytes).
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
    -- | Memoisation slot for scan results. Each 'mkSource' call gets a
    -- fresh 'IORef' so cache entries are scoped to this exact 'Source'
    -- value.
    , srcScanCache :: ScanCacheBox
    }

-- | Build a 'Source' from a file path and its raw bytes.
-- One linear scan over @bs@ builds the line-start table; all subsequent
-- 'offsetToPos' calls pay only O(log n).
--
-- Allocates a fresh per-Source scan cache 'IORef' via plain 'newIORef'.
-- This must run in 'IO' rather than via 'unsafePerformIO' because GHC's
-- optimiser is free to share the result of an 'unsafePerformIO (newIORef
-- ...)' call across calls whose arguments don't appear in the result type
-- — which silently fused all scan caches together and made every cache
-- read return the FIRST source's scan output regardless of which Source
-- was queried.
mkSource :: FilePath -> ByteString -> IO Source
mkSource name bs = do
    cache <- ScanCacheBox <$> newIORef Map.empty
    pure (Source name bs (buildLineStarts bs) cache)

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
readSourceFile path = BS.readFile path >>= mkSource path

-- | Replace the byte content of a 'Source', recomputing the line-start table.
-- Use this instead of record-update on 'srcBytes' so 'offsetToPos' stays valid.
withBytes :: Source -> ByteString -> IO Source
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
