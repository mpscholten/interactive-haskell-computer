import Data.List.NonEmpty (NonEmpty (..))

xs :: NonEmpty Int
xs = 1 :| [2, 3]

listLen [] = 0 :: Int
listLen (_ : ys) = 1 + listLen ys

main = do
    print (case xs of ~(a :| as) -> listLen as)
    print (length xs)
