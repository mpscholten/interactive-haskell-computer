-- ($!) strict application forces to WHNF before passing to function
myId x = x

applyStrict f x = f $! x

main = do
    print (applyStrict myId 42)
    print (applyStrict negate 10)
    print (applyStrict (+1) 99)
