{-# LANGUAGE NamedFieldPuns #-}

data User = User { name :: String, age :: Int } deriving Show

label :: User -> String
label User{ name, age = a } = name ++ "-" ++ show a

main = putStrLn (label (User { name = "Alice", age = 30 }))
