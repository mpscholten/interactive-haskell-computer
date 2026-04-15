{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

classify = \case
    0 -> "zero"
    1 -> "one"
    n -> if | n < 0    -> "negative"
            | n < 10   -> "small"
            | True     -> "large"

main = do
    putStrLn (classify 0)
    putStrLn (classify 1)
    putStrLn (classify (-5))
    putStrLn (classify 7)
    putStrLn (classify 100)
