data Rose = Rose Int [Rose]

roseSize (Rose _ children) = 1 + sumSizes children

sumSizes []     = 0
sumSizes (r:rs) = roseSize r + sumSizes rs

main = do
    let t = Rose 1 [Rose 2 [], Rose 3 [Rose 4 [], Rose 5 []]]
    print (roseSize t)
