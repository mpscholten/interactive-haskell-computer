-- Exercise address-of foreign imports (`foreign import ccall "&sym"`).
-- The scanner strips the leading `&` from the symbol string and the
-- dispatcher short-circuits into a bare `Ptr T` via `resolveSymbol`.
-- Used in the wild by bytestring's lookup tables and `unix` signal
-- constants.  Here we just take the address of libc's `getpid` (always
-- linked in via libSystem) and check the pointer is non-null.
import Foreign.Ptr (Ptr, nullPtr)

foreign import ccall "&getpid" p_getpid :: Ptr ()

main :: IO ()
main =
    if p_getpid == nullPtr
        then putStrLn "fail"
        else putStrLn "ok"
