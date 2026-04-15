applyAll fs x = go fs x
  where
    go []     acc = acc
    go (f:fs) acc = go fs (f acc)

main = do
    let add5 = (+ 5)
    let sub3 = subtract 3
    print (add5 10)
    print (sub3 10)
    print (applyAll [add5, sub3, (+ 1)] 0)
