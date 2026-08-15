-- Col-1 standalone function whose name is a class method and whose
-- RHS is a different-name EVar.  That is a value, not an
-- instance-dictionary alias (`(-) = subtractSize` is indented).
-- Data.Set.map = fromList . List.map f . toList, and
-- toList = toAscList; yielding toList made List.map see a leftover.
import qualified Data.Set as S

main :: IO ()
main = print (S.toAscList (S.map id (S.singleton (1 :: Int))))
