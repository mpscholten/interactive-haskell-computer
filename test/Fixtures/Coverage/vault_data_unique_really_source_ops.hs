import Data.Unique.Really (hashUnique, newUnique)

main :: IO ()
main = do
    x <- newUnique
    y <- newUnique
    print (x == x)
    print (x == y)
    print (hashUnique x == hashUnique x)
    print (hashUnique x == hashUnique y)
