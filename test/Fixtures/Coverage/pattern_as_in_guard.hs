describeList xs@(x:_)
    | x > 0 = "positive head in " ++ show (length xs) ++ "-elem list"
    | True  = "non-positive head"
describeList [] = "empty"

main = do
    putStrLn (describeList [3, 1, 2])
    putStrLn (describeList [])
