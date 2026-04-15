-- OverloadedRecordDot section: (.field) as first-class getter.
data Person = Person { name :: String, age :: Int }

people = [Person { name = "Alice", age = 30 }, Person { name = "Bob", age = 25 }]

map f []     = []
map f (x:xs) = f x : map f xs

main = do
    let ages = map (.age) people
    print ages
