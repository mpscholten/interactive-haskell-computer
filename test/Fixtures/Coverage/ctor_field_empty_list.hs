-- Empty [] in a constructor field is the nil constructor, not
-- OverloadedStrings fromString.  Parser [] and desugared "" share
-- EVar "[]"; wrapping every non-[Char] nil left a leftover function
-- in [Flag] fields.  packBits then died: k `elem` xs saw <function>
-- as the if-condition (getAddrInfo defaultHints / Stream-only hints).
--
-- Structural: unary flags enum + record/positional list field.  No
-- Network.Socket names.
import Data.Bits ((.|.))
import Data.List (foldl')

data Flag = F1 | F2 | F3 deriving (Eq, Show)

data Hints = Hints { flags :: [Flag], n :: Int }

packBits :: (Eq a, Bits b) => [(a, b)] -> [a] -> b
packBits mapping xs = foldl' pack 0 mapping
  where
    pack acc (k, v) | k `elem` xs = acc .|. v
                    | otherwise   = acc

mapping :: [(Flag, Int)]
mapping = [(F1, 1), (F2, 2), (F3, 4)]

defaultHints :: Hints
defaultHints = Hints { flags = [], n = 0 }

posHints :: Hints
posHints = Hints [] 0

main :: IO ()
main = do
    print (flags defaultHints)
    print (F1 `elem` flags defaultHints)
    print (packBits mapping (flags defaultHints))
    print (packBits mapping (flags posHints))
    let updated = defaultHints { n = 1 }
    print (packBits mapping (flags updated))
    print (packBits mapping (flags (defaultHints { flags = [F1], n = 1 })))
    print (F1 `elem` [F1])
    print (F1 `elem` [])
