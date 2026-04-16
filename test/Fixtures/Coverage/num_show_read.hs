-- show on numeric types
main = do
    putStrLn (show (42 :: Int))
    putStrLn (show (-17 :: Int))
    putStrLn (show (0 :: Int))
    -- concatMap show via explicit recursion
    putStrLn (go [1..5 :: Int])

go [] = ""
go (x:xs) = show x ++ go xs
