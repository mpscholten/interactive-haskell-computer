sumList xs = case xs of
    []     -> 0
    (y:ys) -> y + sumList ys

main = print (sumList [1, 2, 3, 4, 5])
