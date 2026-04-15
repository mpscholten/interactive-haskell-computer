myRepeat x = x : myRepeat x

myTake 0 _      = []
myTake _ []     = []
myTake n (x:xs) = x : myTake (n - 1) xs

main = print (myTake 3 (myRepeat 1))
