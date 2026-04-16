sumStrict !acc []     = acc
sumStrict !acc (x:xs) = sumStrict (acc + x) xs

main = print (sumStrict 0 [1..10])
