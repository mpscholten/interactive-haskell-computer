class Describe a where
    name :: a -> String
    age :: a -> Int
    age _ = 0

data Cat = Cat

instance Describe Cat where
    name _ = "whiskers"

main = do
    putStrLn (name Cat)
    print (age Cat)
