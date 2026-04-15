sumStrict !acc []     = acc
sumStrict !acc (x:xs) = sumStrict (acc + x) xs

main = print (sumStrict 0 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
