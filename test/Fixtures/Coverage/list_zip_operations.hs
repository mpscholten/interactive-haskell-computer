-- zip and zipWith
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

main = do
    print (myZip [1..5 :: Int] ['a'..'e'])
    print (myZipWith (+) [1..5 :: Int] [10,20,30,40,50])
    -- unzip manually
    let pairs = [(1,'a'),(2,'b'),(3,'c')] :: [(Int,Char)]
    print (unzipFst pairs)
    print (unzipSnd pairs)

unzipFst [] = []
unzipFst ((a,_):xs) = a : unzipFst xs

unzipSnd [] = []
unzipSnd ((_,b):xs) = b : unzipSnd xs
