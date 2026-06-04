-- | Full socket lifecycle: listen, fork a client, accept, close.
import Control.Concurrent (forkIO, threadDelay)
import Network.Socket

main :: IO ()
main = do
    let port = 18460
    addrs <- getAddrInfo
        (Just defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream })
        (Just "127.0.0.1") (Just (show port))
    let addr = head addrs
    listenSock <- openSocket addr
    setSocketOption listenSock ReuseAddr 1
    bind listenSock (addrAddress addr)
    listen listenSock 5
    _ <- forkIO $ do
        threadDelay 200000
        caddrs <- getAddrInfo
            (Just defaultHints { addrSocketType = Stream })
            (Just "127.0.0.1") (Just (show port))
        let caddr = head caddrs
        csock <- openSocket caddr
        connect csock (addrAddress caddr)
        close csock
    (s, _) <- accept listenSock
    close s
    close listenSock
    putStrLn "OK"
