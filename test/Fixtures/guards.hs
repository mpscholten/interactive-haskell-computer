classify n
    | n < 0     = -1
    | n == 0    = 0
    | otherwise = 1
main = do
    print (classify (-5))
    print (classify 0)
    print (classify 100)
