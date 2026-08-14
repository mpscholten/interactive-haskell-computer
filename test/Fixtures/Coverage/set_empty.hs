-- Data.Set.empty is a facade re-export of empty = Tip, not a col-1
-- binding in Data.Set.  It must not resolve to Alternative.empty
-- (class-method dispatcher).  Set.null / Set.size then match Tip/Bin
-- and would die with args=<function>.
import qualified Data.Set as Set

main :: IO ()
main = do
    print (Set.null Set.empty)
    print (Set.size Set.empty)
