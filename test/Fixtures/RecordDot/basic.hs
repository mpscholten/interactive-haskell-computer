-- OverloadedRecordDot basic: accessor via dot notation.
data Person = Person { name :: String, age :: Int }

main = do
    let p = Person { name = "Alice", age = 30 }
    putStrLn p.name
    print p.age
