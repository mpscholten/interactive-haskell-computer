-- Isolated leftover of many (char 'x') then length xs:
-- import Text.Megaparsec must not install Data.List.NonEmpty.length
-- as bare Foldable.length.  Pre-fix: PCon ":|" [PWild, PVar "xs"] on [].
import Text.Megaparsec

main :: IO ()
main = do
  print (length ([] :: [Int]))
  print (length [1, 2, 3 :: Int])
