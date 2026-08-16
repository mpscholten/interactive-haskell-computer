import Foreign.C.Types (CSize)
import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff)
import Network.Socket.Posix.IOVec (withIOVec)

main :: IO ()
main = allocaBytes 3 $ \first -> allocaBytes 12 $ \second ->
  withIOVec [(first, 3), (second, 12)] $ \(iov, count) -> do
    base1 <- peekByteOff iov 0 :: IO (Ptr Word8)
    len1 <- peekByteOff iov 8 :: IO CSize
    base2 <- peekByteOff (iov `plusPtr` 16) 0 :: IO (Ptr Word8)
    len2 <- peekByteOff (iov `plusPtr` 16) 8 :: IO CSize
    print (base1 == first, len1, base2 == second, len2, count)
