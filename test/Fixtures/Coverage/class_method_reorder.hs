class Describe a where
    name :: a -> String
    age :: a -> Int

data Cat = Cat

instance Describe Cat where
    age _ = 9
    name _ = "whiskers"

main = do
    putStrLn (name Cat)
    print (age Cat)
