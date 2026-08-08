-- Foreign re-exports Data.Bits; qualified F.unsafeShiftR must resolve.
import qualified Foreign as F
import Data.Word (Word32)
main = do
  print (F.unsafeShiftR (255 :: Word32) 4)
  print (F.countLeadingZeros (255 :: Word32))
  print (F.popCount (255 :: Word32))
