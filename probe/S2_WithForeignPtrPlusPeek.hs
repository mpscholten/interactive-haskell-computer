-- Sanity probe 2: withForeignPtr + plusPtr + peek.
-- Allocates a ForeignPtr, pokes bytes, then peeks using plusPtr offsets.
-- This is the same shape as the pattern inside parseRequestLine, minus memchr.
-- Expected: prints bytes 65,66,67,68 (A,B,C,D).

import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, poke)
import Data.Word (Word8)

main :: IO ()
main = do
    putStrLn "before alloc"
    fp <- mallocForeignPtrBytes 8 :: IO (ForeignPtr Word8)
    putStrLn "before withForeignPtr"
    withForeignPtr fp $ \ptr -> do
        -- Poke 4 bytes: 65..68.
        poke (ptr `plusPtr` 0 :: Ptr Word8) (65 :: Word8)
        poke (ptr `plusPtr` 1 :: Ptr Word8) (66 :: Word8)
        poke (ptr `plusPtr` 2 :: Ptr Word8) (67 :: Word8)
        poke (ptr `plusPtr` 3 :: Ptr Word8) (68 :: Word8)
        putStrLn "after poke"
        -- Peek back.
        b0 <- peek (ptr `plusPtr` 0 :: Ptr Word8)
        b1 <- peek (ptr `plusPtr` 1 :: Ptr Word8)
        b2 <- peek (ptr `plusPtr` 2 :: Ptr Word8)
        b3 <- peek (ptr `plusPtr` 3 :: Ptr Word8)
        putStrLn ("b0=" ++ show b0)
        putStrLn ("b1=" ++ show b1)
        putStrLn ("b2=" ++ show b2)
        putStrLn ("b3=" ++ show b3)
    putStrLn "ok"
