-- div/mod vs quot/rem differences with negative numbers
main = do
    -- div rounds toward negative infinity; quot rounds toward zero
    print (div 7 2)
    print (mod 7 2)
    print (quot 7 2)
    print (rem 7 2)
    -- negative cases differ
    print (div (-7) 2)
    print (mod (-7) 2)
    print (quot (-7) 2)
    print (rem (-7) 2)
    -- divMod tuple via case
    case divMod 17 5 of
        (d, m) -> do
            print d
            print m
    -- quotRem tuple via case
    case quotRem 17 5 of
        (q, r) -> do
            print q
            print r
