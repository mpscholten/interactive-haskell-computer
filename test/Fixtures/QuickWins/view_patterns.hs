-- ViewPatterns: pattern (f -> x) binds x to (f scrutinee).
-- Standard IHP usage: view a getter expression then pattern-match the result.
double :: Int -> Int
double n = n + n

classify :: Int -> String
classify (double -> 4) = "double is 4"
classify (double -> 6) = "double is 6"
classify _             = "other"

main = do
    putStrLn (classify 2)
    putStrLn (classify 3)
    putStrLn (classify 5)
