import Network.Socket

main :: IO ()
main = do
    addrs <- getAddrInfo
        (Just defaultHints { addrSocketType = Stream })
        (Just "127.0.0.1")
        (Just "80")
    let addr = head addrs
    print (addrSocketType addr == Stream)
