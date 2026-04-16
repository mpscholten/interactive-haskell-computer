-- Fixture: source-load Data.List from base cache and use sort.
-- Previously short-circuited; should now hit real Data.List source.
import Data.List (sort)

main :: IO ()
main = print (sort [3,1,4,1,5,9,2,6])
