-- mapM_ and forM_ with IO actions
main = do
    mapM_ print [1..5 :: Int]
    mapM_ putStrLn ["alpha", "beta", "gamma"]
    -- sequence_
    sequence_ [print (i*i) | i <- [1..3 :: Int]]
