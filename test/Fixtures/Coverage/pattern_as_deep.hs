-- Deep as-pattern: outer binds the whole list, inner binds the head
-- pair, and pair-fields are further destructured. Complements
-- pattern_as_in_guard.hs which tests a single-level as-pattern.
describe :: [(Int, Int)] -> String
describe whole@(pair@(a, b):_) =
    "first=" ++ show pair
        ++ " sum=" ++ show (a + b)
        ++ " len=" ++ show (length whole)
describe [] = "none"

main :: IO ()
main = do
    putStrLn (describe [(1, 2), (3, 4), (5, 6)])
    putStrLn (describe [])
