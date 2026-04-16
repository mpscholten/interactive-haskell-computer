-- Guarded let bindings: let f x | guard = e | otherwise = e'
classify n =
    let label | n < 0    = "negative"
              | n == 0   = "zero"
              | otherwise = "positive"
    in label

main = do
    putStrLn (classify (-3))
    putStrLn (classify 0)
    putStrLn (classify 7)
