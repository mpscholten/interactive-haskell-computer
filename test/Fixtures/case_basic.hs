classify n = case n of
    0 -> 0
    1 -> 1
    2 -> 1
    _ -> 99

main = do
    print (classify 0)
    print (classify 1)
    print (classify 2)
    print (classify 5)
-- 0 1 1 99
