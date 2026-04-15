myMax x y = if x >= y then x else y

myMin x y = if x <= y then x else y

clamp lo hi x = myMax lo (myMin hi x)

main = do
    print (myMax 3 7)
    print (myMin 3 7)
    print (clamp 0 10 15)
    print (clamp 0 10 (-5))
    print (clamp 0 10 5)
