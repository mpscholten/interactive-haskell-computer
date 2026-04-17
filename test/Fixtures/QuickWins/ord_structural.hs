-- Structural Ord fallback for user-derived data types.
--
-- Builtins.ordCmp walks VCon values field-by-field when no user Ord
-- instance is registered, mirroring the existing eqVals fallback. For
-- cross-constructor ordering the DataRegistry carries the 0-based
-- declaration index of every constructor, so @Red < Green < Blue@
-- matches GHC's derived-Ord semantics.
import Data.List (sort)

data Color = Red | Green | Blue deriving (Eq, Ord, Show)

-- Product type: same-constructor field-compare → canonical derived-Ord.
data Pair = Pair Int Int deriving (Eq, Ord, Show)

main :: IO ()
main = do
    -- Sum type: cross-constructor ordering by declaration index.
    print (compare Red Blue)          -- Red (0) < Blue (2) → LT
    print (Red < Blue)                -- True
    print (max Red Blue)              -- Blue
    print (sort [Blue, Red, Green])   -- [Red,Green,Blue]
    -- Product type: same ctor, field-lex-compare.
    print (compare (Pair 1 2) (Pair 1 3))   -- LT
    print (compare (Pair 1 2) (Pair 1 2))   -- EQ
    print (compare (Pair 2 0) (Pair 1 99))  -- GT
    -- Bool is hardcoded: False < True.
    print (compare False True)        -- LT
    print (min True False)            -- False
