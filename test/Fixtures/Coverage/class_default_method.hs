-- Class with default method that is overridden in one instance but not another
class Describable a where
    describe :: a -> String
    shortDesc :: a -> String
    shortDesc x = take3 (describe x)

take3 [] = []
take3 (x:xs) = x : take3' 2 xs
take3' 0 _ = []
take3' n [] = []
take3' n (x:xs) = x : take3' (n-1) xs

data Color = Red | Green | Blue

instance Describable Color where
    describe Red   = "Red color"
    describe Green = "Green color"
    describe Blue  = "Blue color"

data Flag = Flag String

instance Describable Flag where
    describe (Flag s)  = "Flag: " ++ s
    shortDesc (Flag s) = "F:" ++ take1 s

take1 [] = []
take1 (x:_) = [x]

main = do
    putStrLn (describe Red)
    putStrLn (shortDesc Red)
    putStrLn (describe (Flag "USA"))
    putStrLn (shortDesc (Flag "USA"))
