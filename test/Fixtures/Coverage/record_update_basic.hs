data Person = Person { name :: String, age :: Int } deriving Show
main = do
    let alice = Person { name = "Alice", age = 30 }
    let older = alice { age = age alice + 1 }
    print (age older)
