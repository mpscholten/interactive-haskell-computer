-- let binding feeding into case scrutinee
categorize n =
    let doubled = n * 2
    in case doubled of
        0  -> "zero"
        2  -> "one doubled"
        4  -> "two doubled"
        _  -> "other"

main = do
    putStrLn (categorize 0)
    putStrLn (categorize 1)
    putStrLn (categorize 2)
    putStrLn (categorize 99)
