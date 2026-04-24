-- Network.Socket smoke test: socket -> close under withSocketsDo + bracket.
-- Unit 8 of the warp hello-world probe matrix.
--
-- Locks in the contract between the host-backed `socket` primop (which
-- returns a `VCon "Socket" [refT, fdT]`) and the source-loaded `close`
-- in `Network.Socket.Types`: `close` goes through `invalidateSocket`,
-- which swaps the fd IORef to -1 and then calls `c_close` via libffi.
-- Regressions in either half (primop shape drift, or IORef/FFI
-- plumbing) show up here.
import Network.Socket
import Control.Exception (bracket)

main :: IO ()
main = do
    putStrLn "before"
    withSocketsDo $ bracket (socket AF_INET Stream 0) close $ \_s ->
        putStrLn "ok"
    putStrLn "done"
