-- Integer arithmetic: large numbers, abs, signum, negate
main = do
    print (abs (-42))
    print (abs 42)
    print (signum (-5))
    print (signum 0)
    print (signum 3)
    print (negate 7)
    print (negate (-3))
    -- min/max
    print (min 3 5)
    print (max 3 5)
    -- large numbers
    print (2 ^ 30 :: Int)
    print (factorial 10)

factorial 0 = 1
factorial n = n * factorial (n - 1)
