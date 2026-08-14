-- AF_INET / AF_INET6 are `pattern AF_* = Family n` (newtype CInt).
-- bindPortTCP filters getAddrInfo results with `addrFamily x /= AF_INET6`.
import Network.Socket (AF_INET, AF_INET6)

main = do
    print (AF_INET == AF_INET)
    print (AF_INET == AF_INET6)
    print (AF_INET /= AF_INET6)
