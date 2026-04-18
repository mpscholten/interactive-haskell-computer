-- Negative literals in case patterns. Must be parenthesised: `(-5)`
-- matches the literal -5 (bare `-5` is parsed as a subtraction).
describe :: Int -> String
describe x = case x of
    (-5) -> "neg five"
    (-1) -> "neg one"
    0    -> "zero"
    1    -> "one"
    _    -> "other"

main :: IO ()
main = do
    putStrLn (describe (-5))
    putStrLn (describe (-1))
    putStrLn (describe 0)
    putStrLn (describe 7)
