eqList []     []     = True
eqList (x:xs) (y:ys) = x == y && eqList xs ys
eqList _      _      = False

main = do
    print (eqList [[1,2],[3,4]] [[1,2],[3,4]])
    print (eqList [[1,2],[3,4]] [[1,2],[3,5]])
    print (eqList ([] :: [Int]) [])
