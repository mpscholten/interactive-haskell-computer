-- OverloadedRecordDot inside a larger expression.
data Person = Person { name :: String, age :: Int }

greet p = "Hello, " ++ p.name ++ "!"

main = do
    let p = Person { name = "Alice", age = 30 }
    putStrLn (greet p)
