-- End-to-end: the EXACT shape of http-types' methodArray, now fully working.
--
--   methodArray :: Array StdMethod Method
--   methodArray = listArray (minBound, maxBound) $ map (B8.pack . show) [minBound :: StdMethod .. maxBound]
--   renderStdMethod m = methodArray ! m
--
-- Exercises all the Array StdMethod fixes together:
--   * the bounds (minBound, maxBound) resolve to M via signature-directed
--     elaboration — which now COMMITS even though the element-list `maxBound`
--     stays ambiguous, because applyMethodSubst reverts that single node to a
--     bare EVar instead of poisoning the whole rewrite (all-or-nothing gate);
--   * the element list [minBound :: M .. maxBound] parses correctly (the `::`
--     no longer swallows the `..`) and derived, VInt-tolerant enumFromTo yields
--     all constructors;
--   * derived Ix + the listArray fill (GHC.Prim unboxed-operator fixities) fill
--     and index every slot, including the last.
import Data.Array (Array, listArray, (!))
import Data.Ix (Ix)

data M = GET | POST | HEAD | PUT | DELETE
    deriving (Show, Eq, Ord, Enum, Bounded, Ix)

methodArray :: Array M String
methodArray = listArray (minBound, maxBound) $ map show [minBound :: M .. maxBound]

main :: IO ()
main = do
    putStrLn (methodArray ! GET)     -- first
    putStrLn (methodArray ! HEAD)    -- middle
    putStrLn (methodArray ! DELETE)  -- last (element previously dropped / mis-typed)
