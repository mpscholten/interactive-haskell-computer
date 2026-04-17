-- Data.List: zip, unzip
--
-- Complements `list_zip_operations.hs` (which tests zipWith) with the
-- plain zip/unzip pair, imported explicitly from Data.List.
--
-- NOTE: `concat`/`concatMap` currently fail with unbound `build`, and
-- `sortOn` LoopExceptions — so they're not exercised in this fixture.
import Data.List (zip, unzip)

main :: IO ()
main = do
    print (zip [1, 2, 3 :: Int] "abc")
    print (unzip [(1, 'a'), (2, 'b'), (3 :: Int, 'c')])
    -- zip truncates at the shorter list
    print (zip [1, 2, 3, 4, 5 :: Int] "ab")
