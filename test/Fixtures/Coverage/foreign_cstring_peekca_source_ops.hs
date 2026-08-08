import Data.Word (Word8)
import Foreign.C.String (CChar, peekCAString)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)

main :: IO ()
main = do
    p <- mallocBytes 3 :: IO (Ptr CChar)
    pokeByteOff p 0 (104 :: Word8)
    pokeByteOff p 1 (105 :: Word8)
    pokeByteOff p 2 (0 :: Word8)
    s <- peekCAString p
    putStrLn s
    free p
