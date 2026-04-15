describe []
    | True = 0
describe (x:_)
    | x > 0  = 1
    | x < 0  = -1
    | otherwise = 2

main = do
    print (describe [])
    print (describe [5,6])
    print (describe [-3])
    print (describe [0,9])
