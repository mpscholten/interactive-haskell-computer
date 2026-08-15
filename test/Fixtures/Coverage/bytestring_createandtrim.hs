-- createAndTrim fill callback: first statement is leftover State# VFun
-- (copyBytes after coerce). evalDo must treat that as IO when the do
-- carrier is IO, then trim (len < max) via memcpyFp / copyBytes.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Storable (poke)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)

main :: IO ()
main = allocaBytes 1 $ \src -> do
    poke (src :: Ptr Word8) 65
    bs <- BSI.createAndTrim 4 $ \p -> do
        copyBytes (p :: Ptr Word8) src 1
        return 1
    print (BS.unpack bs)
