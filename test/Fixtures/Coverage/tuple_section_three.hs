-- TupleSections: two missing elements (first and last)
-- (, 'a', ) desugars to \x y -> (x, 'a', y)
withA = (, 'a', )

getFst t = case t of { (x, _, _) -> x }
getSnd t = case t of { (_, y, _) -> y }
getThd t = case t of { (_, _, z) -> z }

main = do
    let t = withA 1 2
    print (getFst t)
    print (getSnd t)
    print (getThd t)
