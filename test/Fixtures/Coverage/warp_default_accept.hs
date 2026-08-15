-- Warp.Settings.defaultAccept is a library binding of Network.Socket.accept.
-- A stale ffiBuiltinNames pin left that library `accept` as a bare leftover
-- (the host acceptB shim was removed when accept source-loaded). Warp.run
-- therefore listened but never accepted. User-file `import Network.Socket
-- (accept)` + bindPortTCP was already GREEN.
--
-- Close the listen socket from a forked thread so accept returns
-- immediately (error) instead of blocking for a client. A leftover
-- defaultAccept hangs before that error.
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, try)
import Data.Streaming.Network (bindPortTCP)
import Network.Socket (Socket, SockAddr, close)
import Network.Wai.Handler.Warp.Settings (defaultSettings, settingsAccept)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "start" >> hFlush stdout
    sock <- bindPortTCP 18790 "*4"
    putStrLn "listen-ok" >> hFlush stdout
    _ <- forkIO $ threadDelay 300000 >> close sock
    r <- try (settingsAccept defaultSettings sock)
            :: IO (Either SomeException (Socket, SockAddr))
    case r of
        Left _ -> putStrLn "accept-errored"
        Right (p, _) -> close p >> putStrLn "accept-ok"
    putStrLn "done"
