data Person = Person { personName :: String, personAge :: Int }

greet p = case p of
    Person {..} -> "Hi " ++ personName ++ ", age " ++ show personAge

main = do
    let p = Person { personName = "Bob", personAge = 25 }
    putStrLn (greet p)
