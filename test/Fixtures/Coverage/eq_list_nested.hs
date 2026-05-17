-- @Eq [a]@ recurses through element Eq via the source-loaded
-- @instance Eq [a]@; nested @[[Int]]@ exercises the recursion twice;
-- @String@ literal equality goes through the same list instance.
main :: IO ()
main = do
    print (([1,2,3] :: [Int]) == [1,2,3])
    print (([1,2,3] :: [Int]) == [1,2,4])
    print (("abc" :: String) == "abc")
    print (([[1],[2,3]] :: [[Int]]) == [[1],[2,3]])
    print (([[1],[2,3]] :: [[Int]]) /= [[1],[2,4]])
    print (([] :: [Int]) == [])
