-- Structural Ord fallback for user-derived data types.
--
-- Builtins.ordCmp now walks VCon values field-by-field when no user
-- Ord instance is registered, mirroring the existing eqVals fallback.
--
-- NOTE: Cross-constructor ordering currently uses a lexicographic
-- comparison of constructor names as a placeholder for the real
-- constructor index (the DataRegistry loses declaration order).
-- See the TODO(ctor-index) on 'structuralOrdering' in IHC.Builtins.
-- Under the lex placeholder, @Red > Green > Blue@ alphabetically —
-- the opposite of the canonical Haskell derived-Ord order — so the
-- expected outputs below reflect the lexicographic fallback, not the
-- eventual-correct derivation-order semantics.
import Data.List (sort)

data Color = Red | Green | Blue deriving (Eq, Ord, Show)

-- Product type: same-constructor field-compare IS correct (not a
-- placeholder), so Pair exercises the half of the fallback that
-- produces canonical derived-Ord results.
data Pair = Pair Int Int deriving (Eq, Ord, Show)

main :: IO ()
main = do
    -- Sum type: cross-constructor ordering via lex ctor-name fallback.
    print (compare Red Blue)          -- Red > Blue alphabetically → GT
    print (Red < Blue)                -- False (lex)
    print (max Red Blue)              -- Red
    print (sort [Blue, Red, Green])   -- [Blue,Green,Red] (lex-sorted)
    -- Product type: same ctor, field-lex-compare → canonical derived-Ord.
    print (compare (Pair 1 2) (Pair 1 3))   -- LT
    print (compare (Pair 1 2) (Pair 1 2))   -- EQ
    print (compare (Pair 2 0) (Pair 1 99))  -- GT
    -- Bool is hardcoded: False < True.
    print (compare False True)        -- LT
    print (min True False)            -- False
