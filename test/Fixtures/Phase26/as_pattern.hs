firstAndAll xs@(x:_) = (x, xs)
main = print (firstAndAll [1,2,3])
