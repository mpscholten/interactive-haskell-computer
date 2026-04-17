-- Data.List: take, drop, takeWhile, dropWhile, replicate, reverse
--
-- Covers the classic Data.List slicing/replication helpers.
import Data.List (take, drop, takeWhile, dropWhile, replicate, reverse)

main :: IO ()
main = do
    print (take 3 [1 .. 10 :: Int])
    print (drop 3 [1 .. 10 :: Int])
    print (takeWhile (< 5) [1 .. 10 :: Int])
    print (dropWhile (< 5) [1 .. 10 :: Int])
    print (replicate 4 'x')
    print (reverse [1, 2, 3 :: Int])
