myWhen cond action = if cond then action else pure ()

main = do
    myWhen True  (putStrLn "yes")
    myWhen False (putStrLn "no")
    myWhen (1 < 2) (putStrLn "math works")
