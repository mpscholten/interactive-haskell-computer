-- Ord total order on a custom type
data Priority = Low | Medium | High deriving (Eq)

instance Ord Priority where
    compare Low    Low    = EQ
    compare Low    _      = LT
    compare _      Low    = GT
    compare Medium Medium = EQ
    compare Medium _      = LT
    compare _      Medium = GT
    compare High   High   = EQ

instance Show Priority where
    show Low    = "Low"
    show Medium = "Medium"
    show High   = "High"

myMin x y = case compare x y of
    LT -> x
    _  -> y

myMax x y = case compare x y of
    GT -> x
    _  -> y

main = do
    print (Low < Medium)
    print (High > Medium)
    print (compare Low High)
    putStrLn (show (myMin Low High))
    putStrLn (show (myMax Low Medium))
