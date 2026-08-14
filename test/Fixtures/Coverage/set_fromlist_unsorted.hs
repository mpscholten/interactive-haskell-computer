-- Data.Set.fromList of a non-ascending list must use Set.insert, not
-- Data.List.insert / insertBy.  HSX leafs end with "!DOCTYPE", which
-- is out of order after "param"; fromList then does insert x t on a
-- Set Bin.  List insertBy matches [] / (:) and dies with
-- Non-exhaustive [[PWild, x, []], [cmp, x, ys@(y:ys')]].
import qualified Data.Set as Set

main :: IO ()
main = do
    let xs = [ "area", "base", "br", "col", "embed", "hr", "img"
             , "input", "link", "meta", "param", "!DOCTYPE"
             ]
        s = Set.fromList xs
    print (Set.member "!DOCTYPE" s)
    print (Set.size s)
    print (Set.member "input" s)
