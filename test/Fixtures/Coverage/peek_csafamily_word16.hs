-- Expected-type flow from `case family :: CSaFamily` into unannotated
-- peekByteOff.  network's peekSockAddr is:
--   family <- peekByteOff p 0
--   case family :: CSaFamily of   -- type CSaFamily = Word16
-- Without the ascription flowing backward, peekByteOff defaults to a
-- 1- or 4-byte read and AF_INET (2) is mashed with sin_port.
import Data.Word (Word8, Word16)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Storable (peekByteOff, pokeByteOff)

type CSaFamily = Word16

main :: IO ()
main = allocaBytes 16 $ \p -> do
    pokeByteOff p 0 (2 :: Word8)
    pokeByteOff p 1 (0 :: Word8)
    pokeByteOff p 2 (0x34 :: Word8)
    pokeByteOff p 3 (0x12 :: Word8)
    family <- peekByteOff p 0
    case family :: CSaFamily of
        2 -> putStrLn "2"
        n -> print (fromIntegral n :: Int)
