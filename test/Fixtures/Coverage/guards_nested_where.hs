classify score
    | score >= aThreshold = "A"
    | score >= bThreshold = "B"
    | score >= cThreshold = "C"
    | True                = "F"
  where
    aThreshold = 90
    bThreshold = 80
    cThreshold = 70

main = do
    putStrLn (classify 95)
    putStrLn (classify 85)
    putStrLn (classify 75)
    putStrLn (classify 60)
