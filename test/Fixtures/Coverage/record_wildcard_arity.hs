-- | Record wildcard pattern (Con {}) on positional constructors.
-- SockAddrInet{} should match a 2-field constructor without named fields.
data Addr
    = AddrV4 Int Int
    | AddrV6 Int Int Int Int

classify :: Addr -> String
classify AddrV4{} = "v4"
classify AddrV6{} = "v6"

main :: IO ()
main = do
    putStrLn (classify (AddrV4 127 1))
    putStrLn (classify (AddrV6 0 0 0 1))
