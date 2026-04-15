double x = x * 2
addOne x = x + 1

applyTwice f x = f (f x)

main = do
    print $ double 21
    print $ addOne $ double 20
    let f = double . addOne
    print (f 4)
    print (applyTwice double 3)
