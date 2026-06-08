-- Network.Socket.Buffer.sendBuf/recvBuf should be source-loaded. This forces
-- the imported function bindings without performing blocking network IO.
import Network.Socket.Buffer (sendBuf, recvBuf)

main :: IO ()
main =
    sendBuf `seq` recvBuf `seq` putStrLn "ok"
