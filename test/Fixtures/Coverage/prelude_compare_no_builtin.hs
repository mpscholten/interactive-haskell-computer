-- compare without the bare-name builtin shim.
--
-- Previously @("compare", compareDispatch reg)@ in 'IHC.Builtins.builtins'
-- intercepted every call to @compare@ before it ever reached the
-- class-method dispatcher.  That entry was dropped (per CLAUDE.md's
-- "Builtin modules: minimum surface only" rule); the source-loaded
-- @class Ord@ in @GHC.Classes@ now drives dispatch, with
-- 'IHC.Scheduler.hostOrdCompareFallback' as a last-resort host
-- structural / primitive Ord pipeline.
--
-- Lock down the cases the fallback unblocks so a future regression in
-- the dispatcher's flow surfaces here:
--
--   * primitive Int / Float / Char compares (fast path),
--   * @[Char]@ string compare (recurses into Char element compare),
--   * @deriving Ord@ on a sum type (declaration-index ordering),
--   * @deriving Ord@ on a product type (lexicographic field compare),
--   * a user @instance Ord@ providing 'compare' explicitly,
--   * @flip compare@ used by sortBy for a descending sort.
import Data.List (sortBy)

data Color = Red | Green | Blue deriving (Eq, Ord, Show)
data Pair  = Pair Int Int        deriving (Eq, Ord, Show)

data Priority = Low | Medium | High deriving (Eq)
instance Ord Priority where
    compare Low    Low    = EQ
    compare Low    _      = LT
    compare _      Low    = GT
    compare Medium Medium = EQ
    compare Medium _      = LT
    compare _      Medium = GT
    compare High   High   = EQ

main :: IO ()
main = do
    -- Primitives:
    print (compare (3 :: Int) 7)        -- LT
    print (compare (1 :: Int) 1)        -- EQ
    print (compare 'a' 'b')             -- LT
    print (compare 'z' 'z')             -- EQ
    -- String compare recurses through Char compare.
    print (compare "abc" "abd")         -- LT
    print (compare "abc" "abc")         -- EQ
    print (compare "z"   "a")           -- GT
    -- Derived Ord on sum / product types.
    print (compare Red Blue)            -- LT  (Red index < Blue index)
    print (compare (Pair 1 2) (Pair 1 3))   -- LT
    print (compare (Pair 1 2) (Pair 1 2))   -- EQ
    print (compare (Pair 2 0) (Pair 1 99))  -- GT
    -- User instance Ord wins over the host fallback.
    print (compare Low High)            -- LT
    print (compare High Medium)         -- GT
    -- 'flip compare' used by Data.List.sortBy.
    print (sortBy (flip compare) [3, 1, 4, 1, 5, 9, 2, 6 :: Int])
