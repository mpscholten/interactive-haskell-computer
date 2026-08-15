-- createAndTrim else-branch: fill shorter than maxLen reallocates via
-- createFp + memcpyFp (unsafeWithForeignPtr → copyBytes after coerce).
-- Recv of a larger buffer is createAndTrim nbytes $ recvBuf.
-- Pre-fix: the memcpyFp nested do saw first-statement VFun (copyBytes)
-- as ParsecT, so createFp returned VIO / RealWorld# and S.length
-- matched BS _ l against <IO>.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)

main :: IO ()
main = do
    trimmed <- BSI.createAndTrim 16 $ \p -> do
        poke (p :: Ptr Word8) (0x62 :: Word8)
        return 1
    print (BS.length trimmed)
    print (BS.head trimmed)
