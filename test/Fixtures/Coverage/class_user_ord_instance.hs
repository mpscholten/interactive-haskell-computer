data Priority = Low | Medium | High

instance Eq Priority where
    (==) Low    Low    = True
    (==) Medium Medium = True
    (==) High   High   = True
    (==) _      _      = False

toInt Low    = 0
toInt Medium = 1
toInt High   = 2

higherPriority p1 p2 = toInt p1 > toInt p2

main = do
    print (Low == Low)
    print (Low == High)
    print (higherPriority High Low)
    print (higherPriority Low Medium)
