data Person = Person { name :: String, age :: Int }

greetShort p = case p of
    Person { name } -> "Hi " ++ name

main = do
    let p = Person { name = "Alice", age = 30 }
    putStrLn (greetShort p)
