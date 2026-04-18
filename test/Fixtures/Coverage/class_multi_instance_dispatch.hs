-- Three user-defined instances of the same user class. Each type
-- tag must route to its own method body — regression for instance
-- registration replacing earlier entries with placeholders.
class Greet a where
    greet :: a -> String

data En = En
data De = De
data Fr = Fr

instance Greet En where greet _ = "hello"
instance Greet De where greet _ = "hallo"
instance Greet Fr where greet _ = "bonjour"

main :: IO ()
main = do
    putStrLn (greet En)
    putStrLn (greet De)
    putStrLn (greet Fr)
