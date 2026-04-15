isEmpty []     = 1
isEmpty (_:_)  = 0
main = do
    print (isEmpty [])
    print (isEmpty [1,2,3])
