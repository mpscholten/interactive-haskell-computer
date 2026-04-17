-- Data.List: isPrefixOf, isSuffixOf, isInfixOf, zipWith3
--
-- Complements the existing `list_zip_operations.hs` by exercising the
-- prefix/suffix/infix checks and the 3-argument zipWith.
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, zipWith3)

main :: IO ()
main = do
    print ("foo" `isPrefixOf` "foobar")
    print ("bar" `isSuffixOf` "foobar")
    print ("oob" `isInfixOf` "foobar")
    print ("xyz" `isInfixOf` "foobar")
    print (zipWith3 (\a b c -> a + b + c) [1, 2 :: Int] [10, 20] [100, 200])
