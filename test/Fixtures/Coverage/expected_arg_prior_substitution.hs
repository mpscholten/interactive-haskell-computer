same :: a -> a -> a
same x _ = x

main :: IO ()
main = print (same True maxBound)
