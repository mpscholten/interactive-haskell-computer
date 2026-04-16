main :: IO ()
main = print result
  where
    result :: Int
    result = helper 10 + helper 20
    helper :: Int -> Int
    helper x = x + 1
