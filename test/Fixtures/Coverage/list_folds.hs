-- foldl and foldr
myFoldl f z [] = z
myFoldl f z (x:xs) = myFoldl f (f z x) xs

myFoldr f z [] = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

main = do
    print (myFoldl (+) 0 [1..10 :: Int])
    print (myFoldr (:) [] [1..5 :: Int])
    print (myFoldl (flip (:)) [] [1..5 :: Int])
    -- sum via foldl
    print (myFoldl (*) 1 [1..6 :: Int])
