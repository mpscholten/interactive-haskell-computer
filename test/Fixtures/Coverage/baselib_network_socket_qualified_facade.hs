-- Network.Socket public facade names should resolve through the source module
-- even when used qualified. The host-backed leaves live in lower provider
-- modules such as Network.Socket.Syscall and Network.Socket.Types.
import qualified Network.Socket as NS

main :: IO ()
main = do
    putStrLn "before"
    sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
    NS.close sock
    putStrLn "ok"
