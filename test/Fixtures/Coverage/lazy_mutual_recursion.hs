-- Mutual recursion with thunks
isEven 0 = True
isEven n = isOdd (n - 1)

isOdd 0 = False
isOdd n = isEven (n - 1)

main = do
    print (isEven 10)
    print (isOdd 7)
    print (isEven 0)
    print (isOdd 1)
