data NonEmpty a = a :| [a]

constNE ~(a :| as) = 1 :: Int

main = do
    print (case undefined of ~(Just _x) -> 0 :: Int)
    print (constNE undefined)
