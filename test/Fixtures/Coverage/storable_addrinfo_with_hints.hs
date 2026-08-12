-- getAddrInfo / bindPortTCP poke defaultHints via Storable AddrInfo.
-- Default pokeByteOff = poke (ptr `plusPtr` off) used to reject the
-- AddrInfo value ("not an Int"). Host pokeByteOff must write the
-- struct addrinfo hint fields, matching poke.
import Network.Socket

main :: IO ()
main = do
    addrs <- getAddrInfo
        (Just defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] })
        (Just "127.0.0.1")
        (Just "80")
    let addr = head addrs
    print (addrSocketType addr == Stream)
