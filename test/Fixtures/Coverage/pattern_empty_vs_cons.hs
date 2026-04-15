myHead []    = -1
myHead (x:_) = x

myNull []    = True
myNull (_:_) = False

main = do
    print (myHead [])
    print (myHead [10, 20, 30])
    print (myNull [])
    print (myNull [1])
