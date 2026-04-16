-- Either operations: either, Left/Right pattern matching
safeRecip x
    | x == 0    = Left "division by zero"
    | otherwise = Right (1.0 / x :: Double)

describe (Left msg) = "Error: " ++ msg
describe (Right v)  = "OK: " ++ show v

main = do
    putStrLn (describe (safeRecip 2.0))
    putStrLn (describe (safeRecip 0.0))
    -- either function
    let handle = either (\e -> "err:" ++ e) (\v -> "val:" ++ show (v :: Double))
    putStrLn (handle (Left "oops"))
    putStrLn (handle (Right 3.14))
