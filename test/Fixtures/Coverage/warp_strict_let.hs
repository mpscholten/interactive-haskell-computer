{-# LANGUAGE Strict #-}
main :: IO ()
main = do
    putStrLn "before"
    let x = 42
    print x
    putStrLn "after"
