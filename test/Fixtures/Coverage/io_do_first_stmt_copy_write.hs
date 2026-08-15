-- First-statement State#-shaped VFun in an IO do must run as IO.
-- Warp runSettings: setSocketCloseOnExec / copyStatus start as leftover
-- VFun; treating them as ParsecT leftover-returns the rest of the do
-- (listen happens, before-loop never runs).  No Warp names.
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import GHC.Storable (writeWord8OffPtr)

toChars bs = map (toEnum . fromIntegral) (BS.unpack bs)

copyPtr :: Ptr Word8 -> ByteString -> IO (Ptr Word8)
copyPtr ptr (PS fp o l) = withForeignPtr fp $ \p -> do
    copyBytes ptr (p `plusPtr` o) (fromIntegral l)
    return (ptr `plusPtr` l)

main = do
  let src = BS.pack [72,84,84,80,47,49,46,49,32]
  bs <- BSI.create 16 $ \ptr -> do
    ptr1 <- copyPtr ptr src
    writeWord8OffPtr ptr1 0 (50 :: Word8)
    writeWord8OffPtr ptr1 1 (48 :: Word8)
    writeWord8OffPtr ptr1 2 (48 :: Word8)
  putStrLn (toChars (BS.take 12 bs))
