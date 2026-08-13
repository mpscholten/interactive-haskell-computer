-- bindPortTCP HostIPv4Only ("*4") calls
--   getAddrInfo (Just hints{AI_PASSIVE, Stream}) Nothing (Just port)
-- GetAddrInfo.getAddrInfo is result-polymorphic (IO (t AddrInfo)).
-- A leftover class dispatcher returns a function; filter/tryAddrs
-- then die with "bindPort: addrs is empty".  Must run getAddrInfoList
-- and return at least one AF_INET (or AF_INET6) address.
import Network.Socket

main :: IO ()
main = do
    let hints = defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    addrs <- getAddrInfo (Just hints) Nothing (Just "3098")
    case addrs of
        []    -> putStrLn "empty"
        (_:_) -> putStrLn "nonempty"
