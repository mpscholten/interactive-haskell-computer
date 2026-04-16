-- Case with guards inside alternatives
classify n = case n of
    x | x < 0    -> "negative"
      | x == 0   -> "zero"
      | x < 10   -> "small"
      | x < 100  -> "medium"
      | otherwise -> "large"

main = do
    putStrLn (classify (-5))
    putStrLn (classify 0)
    putStrLn (classify 7)
    putStrLn (classify 42)
    putStrLn (classify 200)
