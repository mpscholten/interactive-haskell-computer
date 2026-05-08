-- Data.ByteString.Internal.Type.minusForeignPtr: pointer arithmetic
-- between two ForeignPtrs derived from the same buffer.
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes)
import GHC.ForeignPtr (plusForeignPtr)
import Data.ByteString.Internal.Type (minusForeignPtr)
import Data.Word (Word8)

main :: IO ()
main = do
    fp <- mallocForeignPtrBytes 16 :: IO (ForeignPtr Word8)
    let fp2 = fp `plusForeignPtr` 5
    print (minusForeignPtr fp2 fp)
