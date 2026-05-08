-- Data.ByteString.Internal.create exercises the IO ()-fill primitive:
-- allocate a 4-byte buffer, poke 4 bytes into it, get back a ByteString.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke)
import Data.Word (Word8)

main :: IO ()
main = do
    bs <- BSI.create 4 $ \p -> do
        poke (p :: Ptr Word8) 104                   -- 'h'
        poke (p `plusPtr` 1 :: Ptr Word8) 105        -- 'i'
        poke (p `plusPtr` 2 :: Ptr Word8) 33         -- '!'
        poke (p `plusPtr` 3 :: Ptr Word8) 33         -- '!'
    print bs
    print (BS.length bs)
