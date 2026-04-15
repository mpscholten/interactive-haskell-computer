classify pair = case pair of
    (0, 0) -> "origin"
    (x, 0) -> "x-axis"
    (0, y) -> "y-axis"
    (x, y) -> "general"

main = do
    putStrLn (classify (0, 0))
    putStrLn (classify (3, 0))
    putStrLn (classify (0, 5))
    putStrLn (classify (2, 7))
