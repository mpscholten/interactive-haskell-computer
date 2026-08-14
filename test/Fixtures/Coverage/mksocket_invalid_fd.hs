-- socket() after c_socket does mkSocket fd (IORef + mkWeakIORef).
-- Isolated from bindPort / getAddrInfo so a hang is the Weak/IORef path.
import Foreign.C.Types (CInt(..))
import Network.Socket.Types (mkSocket)

main = do
    _ <- mkSocket (CInt (-1))
    putStrLn "mk-ok"
