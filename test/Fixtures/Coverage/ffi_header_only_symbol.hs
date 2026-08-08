-- | FFI: when the symbol string is just a header name (e.g. "string.h"),
-- the C symbol should be taken from the Haskell identifier, not the header.
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

foreign import ccall unsafe "string.h" memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)

main :: IO ()
main = do
    ptr <- mallocBytes 4
    _ <- memset ptr 42 4
    b <- peekByteOff ptr 0 :: IO CChar
    print (fromIntegral b :: Int)
    free ptr
