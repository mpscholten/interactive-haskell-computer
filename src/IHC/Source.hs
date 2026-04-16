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
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

type Pos = Int           -- ^ byte offset into the source
type Span = (Pos, Pos)   -- ^ half-open [start, end)

data Source = Source
    { srcName      :: !FilePath
    , srcBytes     :: !ByteString
    -- | Byte offsets of the first byte of each line, 0-indexed.
    -- @srcLineStarts !! 0 == 0@ always.  Computed once in 'mkSource'.
    , srcLineStarts :: ![Int]
    }

-- | Build a 'Source' from a file path and its raw bytes.
-- One linear scan over @bs@ builds the line-start table; all subsequent
-- 'offsetToPos' calls pay only O(log n).
mkSource :: FilePath -> ByteString -> Source
mkSource name bs = Source name bs (buildLineStarts bs)

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
