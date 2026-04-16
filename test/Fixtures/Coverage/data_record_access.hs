-- Record syntax: construction and pattern match access (field accessors are XFAIL)
data Person = Person
    { firstName :: String
    , lastName  :: String
    , personAge :: Int
    }

getFirst (Person f _ _) = f
getLast  (Person _ l _) = l
getAge   (Person _ _ a) = a

fullName p = getFirst p ++ " " ++ getLast p

birthday (Person f l a) = Person f l (a + 1)

main = do
    let p = Person { firstName = "John", lastName = "Doe", personAge = 30 }
    putStrLn (fullName p)
    print (getAge p)
    let p2 = birthday p
    print (getAge p2)
    putStrLn (getFirst p2)
