-- Guards whose conditions reference where-bound names. Exercises
-- the evaluator path where each guard pulls values from the shared
-- where-scope without re-evaluating the scrutinee.
classify :: Int -> String
classify n
    | isSmall    = "small"
    | isMedium   = "medium"
    | otherwise  = "large"
  where
    isSmall  = n < 10
    isMedium = n < 100

main :: IO ()
main = do
    putStrLn (classify 3)
    putStrLn (classify 50)
    putStrLn (classify 500)
