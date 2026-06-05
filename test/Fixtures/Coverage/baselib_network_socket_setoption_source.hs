-- Regression for Warp bind setup: Network.Socket.Options.setSocketOption
-- source-loads through setSockOpt, withFdSocket, and a foreign import whose
-- generated wrapper pattern-matches the fd as Fd.  IHC's host socket stores
-- the fd as a raw Int, so matchPat must make the Fd newtype transparent.
import Network.Socket

main :: IO ()
main = do
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    putStrLn "ok"
    close sock
