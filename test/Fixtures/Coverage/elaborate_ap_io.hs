main = do
    r <- (pure (\() -> 7) :: IO (() -> Int)) <*> putStrLn "ap-io-effect"
    print r
