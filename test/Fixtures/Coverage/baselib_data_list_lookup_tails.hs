-- Data.List: elem, notElem, lookup, tails
--
-- Covers the membership / association-list / suffix-list functions.
import Data.List (elem, notElem, lookup, tails)

main :: IO ()
main = do
    print (elem (3 :: Int) [1, 2, 3, 4])
    print (notElem (5 :: Int) [1, 2, 3, 4])
    print (lookup (2 :: Int) [(1, "a"), (2, "b"), (3, "c")])
    print (lookup (9 :: Int) [(1, "a"), (2, "b")])
    print (tails [1, 2, 3 :: Int])
