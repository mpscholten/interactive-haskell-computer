-- User-defined Eq instance on a small sum ADT. Exercises instance
-- method registration for the canonical `(==)` operator and its
-- fallback `(/=)` default.
data Color = Red | Green | Blue

instance Eq Color where
    Red   == Red   = True
    Green == Green = True
    Blue  == Blue  = True
    _     == _     = False

main :: IO ()
main = do
    print (Red == Red)
    print (Red == Green)
    print (Blue == Blue)
    print (Green /= Red)
