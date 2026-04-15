len []     = 0
len (_:xs) = 1 + len xs

main = do
    print (len [])
    print (len [10,20,30,40,50])
