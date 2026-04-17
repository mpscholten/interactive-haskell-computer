class Describe a where
    name :: a -> String

data Cat = Cat

helperName = "whiskers"

instance Describe Cat where
    name _ = helperName
    helper _ = "ignored"

main = putStrLn (name Cat)
