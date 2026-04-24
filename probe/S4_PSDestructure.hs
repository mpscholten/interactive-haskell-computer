-- Sanity probe 4: destructure a ByteString via the PS pattern synonym
-- and inspect the inner ForeignPtr/offset/length. Mirrors the opening
-- of parseRequestLine's destructuring step.
-- Expected: prints the underlying bytes one by one.

import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Internal (ByteString(PS))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peek)
import Data.Word (Word8)

main :: IO ()
main = do
    let bs = C8.pack "GET "
    putStrLn ("len=" ++ show (C8.length bs))
    case bs of
        PS fp off len -> do
            putStrLn ("PS off=" ++ show off ++ " len=" ++ show len)
            withForeignPtr fp $ \ptr -> do
                let p = ptr `plusPtr` off :: Ptr Word8
                b0 <- peek (p `plusPtr` 0 :: Ptr Word8)
                b1 <- peek (p `plusPtr` 1 :: Ptr Word8)
                b2 <- peek (p `plusPtr` 2 :: Ptr Word8)
                b3 <- peek (p `plusPtr` 3 :: Ptr Word8)
                putStrLn ("b0=" ++ show b0)  -- 71 'G'
                putStrLn ("b1=" ++ show b1)  -- 69 'E'
                putStrLn ("b2=" ++ show b2)  -- 84 'T'
                putStrLn ("b3=" ++ show b3)  -- 32 ' '
    putStrLn "ok"
