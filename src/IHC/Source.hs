-- | Immutable source buffer + cheap byte-offset cursors.
--
-- A @Source@ is the whole file's bytes plus a filename. All position
-- tracking is done with @Int@ byte offsets; line/column are recovered lazily
-- when we need to report a diagnostic.
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
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

type Pos = Int           -- ^ byte offset into the source
type Span = (Pos, Pos)   -- ^ half-open [start, end)

data Source = Source
    { srcName  :: !FilePath
    , srcBytes :: !ByteString
    }

mkSource :: FilePath -> ByteString -> Source
mkSource = Source

readSourceFile :: FilePath -> IO Source
readSourceFile path = Source path <$> BS.readFile path

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
