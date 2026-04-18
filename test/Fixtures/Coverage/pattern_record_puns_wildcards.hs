-- Record pattern combining a punned field (`name`) with field wildcards
-- (`..`). Verifies that both extensions co-exist inside a single pattern.
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
data Person = Person { name :: String, age :: Int, city :: String }

greet :: Person -> String
greet Person { name, .. } = "Hi " ++ name ++ " from " ++ city ++ "!"

ageOnly :: Person -> Int
ageOnly Person { age } = age

main :: IO ()
main = do
    let p = Person { name = "Ada", age = 30, city = "London" }
    putStrLn (greet p)
    print (ageOnly p)
