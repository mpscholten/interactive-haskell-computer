import Data.Word (Word8)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, peekByteOff, poke, pokeByteOff)

main :: IO ()
main =
    allocaBytes 4 $ \raw -> do
        let p = castPtr raw :: Ptr Word8
        poke p 65
        a <- peek p :: IO Word8
        pokeByteOff p 1 (66 :: Word8)
        b <- peekByteOff p 1 :: IO Word8
        print a
        print b
