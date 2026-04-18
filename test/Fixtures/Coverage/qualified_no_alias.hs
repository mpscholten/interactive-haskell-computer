-- `import qualified Data.List` (no alias) followed by `Data.List.foo`.
-- Exercises the full-module-name qualifier path in the renamer.
import qualified Data.List

main :: IO ()
main = do
    print (Data.List.sort [5, 2, 4, 1 :: Int])
    print (Data.List.length [10, 20, 30 :: Int])
