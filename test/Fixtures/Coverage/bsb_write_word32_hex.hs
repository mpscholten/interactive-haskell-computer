{-# LANGUAGE BangPatterns #-}
import Control.Monad (when)
import Data.Bits ((.&.), unsafeShiftR)
import Data.Word (Word8, Word32)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek, poke)
import Network.Wai.Handler.Warp ()

writeHex :: Int -> Word32 -> Ptr Word8 -> IO (Ptr Word8)
writeHex len w0 op0 = do
    go w0 (op0 `plusPtr` (len - 1))
    pure $ op0 `plusPtr` len
  where
    go !w !op = when (op >= op0) $ do
        let nibble = fromIntegral w .&. 0xF :: Word8
            hex | nibble < 10 = 48 + nibble
                | otherwise = 55 + nibble
        poke op hex
        go (w `unsafeShiftR` 4) (op `plusPtr` (-1))

main :: IO ()
main = allocaBytes 8 $ \ptr -> do
    _ <- writeHex 4 12 ptr
    a <- peek ptr :: IO Word8
    b <- peek (ptr `plusPtr` 1) :: IO Word8
    c <- peek (ptr `plusPtr` 2) :: IO Word8
    d <- peek (ptr `plusPtr` 3) :: IO Word8
    print (a, b, c, d)
