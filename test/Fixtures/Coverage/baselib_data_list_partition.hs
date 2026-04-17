-- Data.List: nub, group, partition, intercalate
--
-- Exercises four list-manipulation functions that live in `Data.List`'s
-- source (re-exported from `Data.OldList`). Complements the existing
-- `sort_via_data_list.hs` QuickWin with the non-sort functions.
import Data.List (nub, group, partition, intercalate)

main :: IO ()
main = do
    print (nub [1, 2, 1, 3, 2, 4 :: Int])
    print (group [1, 1, 2, 3, 3, 3 :: Int])
    print (partition (< 3) [1, 2, 3, 4, 5 :: Int])
    putStrLn (intercalate ", " ["a", "b", "c"])
