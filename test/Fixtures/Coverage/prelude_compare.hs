-- @compare@ source-loaded from @GHC.Classes@.
--
-- The bare-name @compare@ shim was dropped from @IHC.Builtins.builtins@
-- per CLAUDE.md's "Builtin modules: minimum surface only" rule.
-- Resolution now flows through the source-loaded @class Ord@ in
-- @ghc-prim/GHC/Classes.hs@:
--
--   * 'IHC.Scheduler.loadProgramFromSource' eagerly loads
--     @GHC.Classes@ (the canonical home of @class Eq@/@Ord@ and the
--     primitive @Ord Int@ / @Ord Char@ / @Ord [a]@ instances).
--   * 'registerGlobalLoadedModule' merges every loaded module's
--     @scanClassDecls@ output into 'globalMethodClassRef', so the
--     env-fallback's 'tryClassMethodFromRegistry' synthesises a
--     'classMethodDispatcher' for @compare@ on demand.
--   * The dispatcher tries the @Ord T.compare@ instance first; on
--     miss it consults the class default body
--     @compare x y = if x == y then EQ else if x <= y then LT else GT@,
--     which routes through the still-builtin @==@ / @<=@ dispatchers
--     (those keep their VCon structural fallback for derived types).
--
-- Lock down the cases the source path covers:
--
--   * primitive Int / Char / Bool compares (uses @Ord T.compare@
--     instance method @compareInt@ etc., or default body via
--     @==@/@<=@),
--   * @[Char]@ string compare (recurses into Char element compare via
--     the @Ord [a]@ source instance @compare (x:xs) (y:ys) = ...@),
--   * @deriving Ord@ on a sum type (declaration-index ordering via
--     @ordDispatch@'s structural fallback driven by the default
--     @compare@ body),
--   * @deriving Ord@ on a product type (lexicographic field compare),
--   * a user @instance Ord@ providing 'compare' explicitly (custom
--     instance wins over the source default),
--   * @flip compare@ used by @sortBy@ for a descending sort.
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
    -- Primitives.
    print (compare (3 :: Int) 7)        -- LT
    print (compare (1 :: Int) 1)        -- EQ
    print (compare 'a' 'b')             -- LT
    print (compare 'z' 'z')             -- EQ
    -- String compare recurses through Char.
    print (compare "abc" "abd")         -- LT
    print (compare "abc" "abc")         -- EQ
    print (compare "z"   "a")           -- GT
    -- Derived Ord on sum / product types.
    print (compare Red Blue)            -- LT
    print (compare (Pair 1 2) (Pair 1 3))   -- LT
    print (compare (Pair 1 2) (Pair 1 2))   -- EQ
    print (compare (Pair 2 0) (Pair 1 99))  -- GT
    -- User instance wins over default.
    print (compare Low High)            -- LT
    print (compare High Medium)         -- GT
    -- @flip compare@ via Data.List.sortBy.
    print (sortBy (flip compare) [3, 1, 4, 1, 5, 9, 2, 6 :: Int])
