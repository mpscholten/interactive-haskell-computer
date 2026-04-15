take n xs = case xs of
    []     -> []
    (y:ys) -> if n <= 0 then [] else y : take (n - 1) ys

main = print (take 3 [10, 20, 30, 40, 50])
