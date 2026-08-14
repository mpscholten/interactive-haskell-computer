-- Data.ByteString.Internal.create used to hang after the poke
-- callback: wrapAction = flip withForeignPtr returns source
-- @IO $ \s -> keepAlive# …@ (VCon "IO"), and evalDo only fast-pathed
-- host VIO. The subsequent @>>@ defaulted to ParsecT, so the fill
-- ran but create never returned a ByteString.
--
-- Warp composeHeader is @create len $ \ptr -> poke …@.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)

main :: IO ()
main = do
    bs <- BSI.create 1 $ \p -> poke (p :: Ptr Word8) (0 :: Word8)
    print (BS.length bs)
