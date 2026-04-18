main :: IO ()
main = do
    putStrLn "before"
    let x = 5 :: Int
    let y = x + 1 :: Int
    print y
    putStrLn "after"
