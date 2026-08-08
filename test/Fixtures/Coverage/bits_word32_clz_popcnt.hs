-- Word32 FiniteBits / Bits bottom out on clz32# / popCnt32#.
import Data.Bits
import Data.Word
main = do
  print (countLeadingZeros (255 :: Word32))
  print (popCount (255 :: Word32))
  print ((255 :: Word32) `unsafeShiftR` 4)
