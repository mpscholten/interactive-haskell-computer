-- Nested constructor pattern: Just (Left x), Just (Right s), Nothing.
-- Exercises multi-level pattern decomposition on two different ctors
-- under the same outer constructor.
peel :: Maybe (Either Int String) -> String
peel (Just (Left n))  = "left " ++ show n
peel (Just (Right s)) = "right " ++ s
peel Nothing          = "none"

main :: IO ()
main = do
    putStrLn (peel (Just (Left 5)))
    putStrLn (peel (Just (Right "hi")))
    putStrLn (peel Nothing)
