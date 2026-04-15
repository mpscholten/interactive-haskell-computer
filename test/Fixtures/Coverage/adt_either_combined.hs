safeDiv _ 0 = Left "division by zero"
safeDiv x y = Right (x `div` y)

showResult (Left e)  = "Error: " ++ e
showResult (Right v) = "Ok: " ++ show v

main = do
    putStrLn (showResult (safeDiv 10 2))
    putStrLn (showResult (safeDiv 7 0))
