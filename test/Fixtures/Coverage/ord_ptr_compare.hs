-- Ord Ptr must compare host addresses across VCon "Ptr" and
-- VPrimObj PrimPtr representations (same carve-out as derived Eq Ptr).
-- Required by bsb-http-chunked writeWord32Hex' (@when (op >= op0)@).
import Foreign.Ptr (nullPtr, plusPtr, Ptr)
import Data.Word (Word8)

main :: IO ()
main = do
    let p0 = nullPtr :: Ptr Word8
        p1 = p0 `plusPtr` 1
        p2 = p0 `plusPtr` 2
    print (p0 == p0)
    print (p1 > p0)
    print (p0 >= p0)
    print (p1 < p2)
    print (compare p2 p0 == GT)
