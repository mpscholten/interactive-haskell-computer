showIfEq x y =
    if x == y
    then "equal: " ++ show x
    else "not equal: " ++ show x ++ " vs " ++ show y

main = do
    putStrLn (showIfEq (42 :: Int) 42)
    putStrLn (showIfEq (1 :: Int) 2)
