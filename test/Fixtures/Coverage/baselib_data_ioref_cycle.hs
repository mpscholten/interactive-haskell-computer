-- Data.IORef: full read/write/modify cycle
--
-- Exercises newIORef, writeIORef, readIORef, and modifyIORef'.
-- Complements the existing `io_ioref_counter.hs` with a mixed
-- arithmetic-op sequence.
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')

main :: IO ()
main = do
    ref <- newIORef (0 :: Int)
    writeIORef ref 10
    readIORef ref >>= print
    modifyIORef' ref (+ 1)
    readIORef ref >>= print
    modifyIORef' ref (* 3)
    readIORef ref >>= print
    modifyIORef' ref (subtract 5)
    readIORef ref >>= print
