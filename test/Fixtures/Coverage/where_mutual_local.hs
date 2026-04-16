-- Mutual-like recursion inside a where clause using explicit helper args
collatz n = go n 0
  where
    go 1 steps = steps
    go n steps = go (step n) (steps + 1)
    step m = if even m then m `div` 2 else 3 * m + 1

main = do
    print (collatz 1)
    print (collatz 6)
    print (collatz 27)
