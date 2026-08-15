-- copyBytes then return (plusPtr): BufferPool.copy / composeHeader
-- (`ptr1 <- copy ptr httpVer`).  Leftover State# VFun after copyBytes
-- must sequence as IO so the bind is a Ptr, not leftover <function>.
-- Explicit >> of the same shape (not a copyBytes/plusPtr name list).
import Data.Word (Word8)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr

copyPtr :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Ptr Word8)
copyPtr dst src l = do
    copyBytes dst src l
    return (dst `plusPtr` l)

copyPtrSeq :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Ptr Word8)
copyPtrSeq dst src l =
    copyBytes dst src l >> return (dst `plusPtr` l)

main :: IO ()
main = do
    dst <- mallocBytes 19
    src <- mallocBytes 9
    ptr1 <- copyPtr dst src 9
    print (ptr1 `minusPtr` dst)
    ptr2 <- copyPtrSeq dst src 9
    print (ptr2 `minusPtr` dst)
