-- Exercise the generic FFI dispatcher with a String→CString marshall.
-- Foreign.C.String.withCString in base reaches for RTS locale state
-- (getForeignEncoding); we bypass that via a host-side withCString that
-- packs each Char as one byte.  This exercises the parse→marshal→call
-- path end-to-end with a pointer argument.
import Foreign.C.String (withCString)
import Foreign.C.Types  (CSize)
import Foreign.Ptr      (Ptr)

foreign import ccall unsafe "strlen" c_strlen :: Ptr CChar -> IO CSize

main :: IO ()
main = do
    n <- withCString "hello" c_strlen
    print n
