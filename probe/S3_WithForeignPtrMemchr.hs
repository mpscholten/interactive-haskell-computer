-- Sanity probe 3: withForeignPtr + memchr via foreign import.
-- Allocates a ForeignPtr, writes "AB CD" into it, then calls memchr to
-- find the space (byte 32). Computes minusPtr to get the offset.
-- Expected: prints "off=2".

import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, minusPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Foreign.C.Types
import Data.Word (Word8)

foreign import ccall unsafe "string.h memchr" c_memchr
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

main :: IO ()
main = do
    putStrLn "before alloc"
    fp <- mallocForeignPtrBytes 8 :: IO (ForeignPtr Word8)
    putStrLn "before withForeignPtr"
    withForeignPtr fp $ \ptr -> do
        -- "AB CD" = 65, 66, 32, 67, 68.
        poke (ptr `plusPtr` 0 :: Ptr Word8) (65 :: Word8)
        poke (ptr `plusPtr` 1 :: Ptr Word8) (66 :: Word8)
        poke (ptr `plusPtr` 2 :: Ptr Word8) (32 :: Word8)
        poke (ptr `plusPtr` 3 :: Ptr Word8) (67 :: Word8)
        poke (ptr `plusPtr` 4 :: Ptr Word8) (68 :: Word8)
        putStrLn "before memchr"
        q <- c_memchr ptr 32 5
        putStrLn "after memchr"
        if q == nullPtr
            then putStrLn "null!"
            else putStrLn ("off=" ++ show (q `minusPtr` ptr))
    putStrLn "ok"
