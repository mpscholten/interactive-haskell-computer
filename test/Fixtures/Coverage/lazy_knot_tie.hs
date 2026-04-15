myTake 0 _      = []
myTake _ []     = []
myTake n (x:xs) = x : myTake (n - 1) xs

main = do
    let xs = 1 : xs
    print (myTake 5 xs)
