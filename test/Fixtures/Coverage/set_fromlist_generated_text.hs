-- Set.fromList of generated T.pack ('x' : show n) texts.
-- compareText does min + Prelude.compare + (<>); those must stay cheap
-- (unboxed Int / Ordering), not ~20ms interpreted class methods each.
import qualified Data.Set as Set
import qualified Data.Text as T

main :: IO ()
main = print (Set.size (Set.fromList (map (\n -> T.pack ('x' : show n)) [1..50 :: Int])))
