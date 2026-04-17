{-# LANGUAGE DuplicateRecordFields #-}
data A = A { x :: Int }
data B = B { x :: String }
main = do
    print (x (A { x = 42 }))
    putStrLn (x (B { x = "hi" }))
