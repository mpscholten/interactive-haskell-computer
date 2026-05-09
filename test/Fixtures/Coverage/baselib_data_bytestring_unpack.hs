-- Data.ByteString.unpack source-loaded path.
-- Source body: unpack bs = build (unpackFoldr bs); unpackFoldr bs k z = foldr k z bs.
-- Data.ByteString.foldr uses pointer arithmetic (`plusPtr`) on a `Ptr` extracted
-- from a `ForeignPtr` via `unsafeForeignPtrToPtr`. Source-loaded `Ptr fo` wraps
-- the host primitive in `VCon "Ptr" [_]`, so `plusPtr`/`minusPtr` need cross-
-- representation handling (mirrors the `eqVals` fix in commit f902b59).
import qualified Data.ByteString as BS

main :: IO ()
main = do
    -- short list
    print (BS.unpack (BS.pack [65, 66, 67]))
    -- ascending bytes including high values
    print (BS.unpack (BS.pack [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 100, 200, 255]))
    -- empty
    print (BS.unpack BS.empty)
    -- lazy take from unpack (build/foldr fusion path)
    print (take 3 (BS.unpack (BS.pack [10, 20, 30, 40, 50])))
    -- length round-trip
    print (length (BS.unpack (BS.pack [1, 2, 3, 4])))
