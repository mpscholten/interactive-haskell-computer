-- TupleSections: (1, , 3) desugars to \x -> (1, x, 3).
-- Use two holes to exercise the multi-hole desugaring too.
main = do
    let f = (1, , 3)
    print (f 2)
    let g = (, , "c")
    print (g 10 20)
