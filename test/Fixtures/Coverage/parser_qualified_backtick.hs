-- Gap: Qualified identifier inside backticks (`` `Mod.fn` ``). Seen in: lens/Internal/TH.hs:2:38, conduit/Lift.hs:1:15. Ref: hackage-parser-gaps.md (lens bucket 11).
import qualified Data.List as L

main = print ([1, 2, 3, 4] `L.intersect` [2, 3, 5])
