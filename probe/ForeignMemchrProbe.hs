{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as S
import Data.ByteString.Internal.Type (ByteString(BS))
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr (unsafeWithForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, nullPtr)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (peek)

foreign import ccall unsafe "string.h memchr" c_memchr
    :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memcmp" c_memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

main :: IO ()
main = do
    let bs = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" :: S.ByteString
    case bs of
        BS fp len -> unsafeWithForeignPtr fp $ \p -> do
            first <- peek p
            putStrLn ("len=" ++ show len)
            putStrLn ("first=" ++ show first)
            cmp <- c_memcmp p (p `plusPtr` 1) 1
            putStrLn ("memcmp-1=" ++ show cmp)
            q0 <- c_memchr p 71 (fromIntegral len)
            putStrLn ("foreign-G-null=" ++ show (q0 == nullPtr))
            putStrLn ("foreign-G-off=" ++ show (q0 `minusPtr` p))
            q <- c_memchr p 10 (fromIntegral len)
            putStrLn ("foreign-null=" ++ show (q == nullPtr))
            putStrLn ("foreign-off=" ++ show (q `minusPtr` p))
