-- Data.List: sortBy, groupBy
--
-- Exercises both higher-order grouping helpers, including `sortBy` with
-- `flip compare` for a descending sort.
import Data.List (sortBy, groupBy)

main :: IO ()
main = do
    -- Descending sort via flip compare
    print (sortBy (flip compare) [3, 1, 4, 1, 5, 9, 2, 6 :: Int])
    -- Group by parity equivalence
    print (groupBy
        (\a b -> a `mod` 2 == b `mod` 2)
        [1, 3, 5, 2, 4, 6, 7, 9 :: Int])
    -- groupBy collapses adjacent equal elements when given (==)
    print (groupBy (==) [1, 1, 2, 2, 2, 3 :: Int])
