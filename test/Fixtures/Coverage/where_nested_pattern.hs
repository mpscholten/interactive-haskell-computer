foo = show (a + b) ++ show (c + d)
  where
    ((a, b), (c, d)) = ((1, 2), (3, 4))

main = putStrLn foo
