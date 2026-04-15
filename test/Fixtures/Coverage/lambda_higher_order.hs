myMap f []     = []
myMap f (x:xs) = f x : myMap f xs

myFilter p []     = []
myFilter p (x:xs)
    | p x       = x : myFilter p xs
    | True      = myFilter p xs

myFoldr f z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

main = do
    print (myMap (\x -> x * 2) [1, 2, 3, 4, 5])
    print (myFilter (\x -> x > 3) [1, 2, 3, 4, 5])
    print (myFoldr (\x acc -> x + acc) 0 [1, 2, 3, 4, 5])
