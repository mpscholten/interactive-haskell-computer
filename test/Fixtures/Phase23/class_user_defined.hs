-- User-defined instance: Eq on a custom type via class registry.
data Color = Red | Green | Blue

instance Eq Color where
    (==) Red   Red   = True
    (==) Green Green = True
    (==) Blue  Blue  = True
    (==) _     _     = False

main = do
    print (Red == Red)
    print (Red == Blue)
    print (Green == Green)
    print (Blue == Red)
