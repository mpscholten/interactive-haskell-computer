-- Regression: using Network.Socket.accept in a do-block bind hangs
-- during discovery.  Discovery of accept's body in Network.Socket.Syscall
-- triggers a cascade through the socket type infrastructure.
import Network.Socket (accept, close)
import Data.Streaming.Network (bindPortTCP)

main :: IO ()
main = do
    sock <- bindPortTCP 8199 "*"
    putStrLn "listening"
    -- Don't actually wait for a connection; just verify discovery completes.
    close sock
    putStrLn "done"
