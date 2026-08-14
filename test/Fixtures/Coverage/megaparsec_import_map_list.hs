-- Isolated leftover of compileToHaskell's unqualified map:
-- import Text.Megaparsec must not install Data.List.NonEmpty.map
-- as bare Prelude.map.  Pre-fix: irrefutable PCon ":|" on a list.
import Text.Megaparsec

main :: IO ()
main = mapM_ print (map (+1) [1, 2, 3 :: Int])
