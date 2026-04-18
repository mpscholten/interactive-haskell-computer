-- `import qualified Data.List as L` + bare top-level reference `L.sort`.
-- Confirms the alias is installed in the module env for top-level use.
import qualified Data.List as L

main :: IO ()
main = do
    print (L.sort [3, 1, 4, 1, 5 :: Int])
    print (L.length [10, 20, 30 :: Int])
    print (L.reverse [1, 2, 3 :: Int])
