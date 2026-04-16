addPair xs = a + b
  where
    (a, b) = xs

main :: IO ()
main = print (addPair (3, 4))
