{-# LANGUAGE ForeignFunctionInterface #-}
-- c_socket takes CInt.  FFI must unwrap the CInt newtype spine to a
-- host integer; a raw VInt-only asInt died with
-- "expected numeric value, got <CInt...>".
import Foreign.C.Types (CInt(..))

foreign import ccall unsafe "socket" c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "close"  c_close  :: CInt -> IO CInt

main :: IO ()
main = do
    fd <- c_socket (CInt 2) (CInt 1) (CInt 0)
    putStrLn (if fd >= 0 then "sock-ok" else "sock-fail")
    _ <- c_close fd
    pure ()
