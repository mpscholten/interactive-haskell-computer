-- S.any on OverloadedStrings ByteString (sanitizeHeaders / containsNewlines).
-- Needs host Word8 Storable.peek for marked buffers + range marks after
-- plusForeignPtr.
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as S
import Data.Word8 (_cr, _lf)

main :: IO ()
main = do
    print (S.any (\w -> w == _cr || w == _lf) ("text/plain" :: S.ByteString))
    print (S.any (== _cr) ("hel\rlo" :: S.ByteString))
    print (S.any (== _lf) ("a\nb" :: S.ByteString))
