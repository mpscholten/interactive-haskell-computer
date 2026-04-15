myMod x y = x - (x `div` y) * y

myGcd a 0 = a
myGcd a b = myGcd b (a `myMod` b)

main = do
    print (10 `myMod` 3)
    print (myGcd 48 18)
    print (15 `div` 4)
