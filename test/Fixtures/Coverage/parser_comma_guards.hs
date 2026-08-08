-- Comma-separated boolean guards on function equations.
-- Seen in: IHP HSX QQ (compileToHaskell), lens ByteString go.
-- Ref: | cond1, cond2 = … must not fail with "expected = after guard; saw TkComma".

classify :: Int -> Int -> String
classify a b
  | a > 0, b > 0 = "both-pos"
  | a < 0, b < 0 = "both-neg"
  | a > 0, b == 0 = "a-pos-b-zero"
  | otherwise = "other"

main = do
    putStrLn (classify 1 2)
    putStrLn (classify (-1) (-3))
    putStrLn (classify 5 0)
    putStrLn (classify 0 1)
    putStrLn (classify (-1) 2)
