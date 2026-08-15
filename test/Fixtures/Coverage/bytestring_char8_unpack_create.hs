-- Isolate Data.ByteString.Char8.unpack of an unsafeCreate Word8
-- buffer — the same ForeignPtr/BS shape formatHTTPDate writes.
-- Date format (http_date_epoch0_format) is GREEN; leftover after
-- tightening http_date_format / http_date_cpy3 off Char8.putStrLn
-- is not unpack (Prelude putStrLn of C8.unpack).
import Data.ByteString.Internal
import qualified Data.ByteString.Char8 as C8
import Data.Word (Word8)
import Foreign.Storable (poke)

main :: IO ()
main = putStrLn (C8.unpack (unsafeCreate 1 $ \p -> poke p (65 :: Word8)))
