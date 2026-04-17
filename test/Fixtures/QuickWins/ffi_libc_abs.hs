-- Exercise the libffi dispatcher with a pure, argument-taking import.
-- `abs` in libc is a no-op wrapper around an integer comparison — good
-- for verifying that the scanner grabs CInt args, the dispatcher
-- marshals them, and the CInt return round-trips back through VInt.
import Foreign.C.Types (CInt)

foreign import ccall unsafe "abs" c_abs :: CInt -> CInt

main :: IO ()
main = do
    print (c_abs (-7))
    print (c_abs 42)
