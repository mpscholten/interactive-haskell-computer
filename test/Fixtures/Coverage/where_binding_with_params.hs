main :: IO ()
main = print (f [1,2,3])

f :: [Int] -> Bool
f xs = go xs
  where
    go []        = True
    go [_]       = True
    go (x:y:ys) = x < y && go (y:ys)
