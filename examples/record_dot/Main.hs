data Address = Address { addrStreet :: String, addrCity :: String }
data Person  = Person  { pName :: String, pAge :: Int, pAddr :: Address }

greet :: Person -> String
greet p =
    "Hello, " ++ p.pName ++ "! Age: " ++ show p.pAge
    ++ ". Lives in " ++ p.pAddr.addrCity
    ++ " on " ++ p.pAddr.addrStreet ++ "."

main :: IO ()
main = do
    let addr = Address { addrStreet = "Baker Street 221B", addrCity = "London" }
    let p1   = Person { pName = "Holmes", pAge = 40, pAddr = addr }
    let p2   = Person { pName = "Watson", pAge = 38, pAddr = addr }
    putStrLn (greet p1)
    putStrLn (greet p2)
    putStrLn ("Same city: " ++ show (p1.pAddr.addrCity == p2.pAddr.addrCity))
