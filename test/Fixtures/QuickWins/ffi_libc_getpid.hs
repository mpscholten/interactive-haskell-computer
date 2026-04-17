-- Smallest possible exercise of the generic `foreign import ccall`
-- dispatcher (IHC.FFI): a zero-argument, integer-returning libc call.
-- The scanner parses the decl below, the scheduler installs a
-- VIO-returning thunk under `c_getpid`, and forcing it calls the real
-- libc getpid() via libffi.  No per-function host builtin involved.
import Foreign.C.Types (CInt)

foreign import ccall unsafe "getpid" c_getpid :: IO CInt

main :: IO ()
main = do
    pid <- c_getpid
    if pid > 0 then putStrLn "ok" else putStrLn "fail"
