classify = \case
    0 -> "zero"
    _ -> "other"
main = do
    putStrLn (classify 0)
    putStrLn (classify 5)
