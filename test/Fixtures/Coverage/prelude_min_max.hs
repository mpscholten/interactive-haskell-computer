-- @min@ / @max@ source-loaded from @GHC.Classes@.
--
-- The bare-name @min@ and @max@ shims were dropped from
-- @IHC.Builtins.builtins@ per CLAUDE.md's "Builtin modules: minimum
-- surface only" rule. Resolution now flows through the source-loaded
-- @class Ord@ defaults in @ghc-prim/GHC/Classes.hs@:
--
--   max x y = if x <= y then y else x
--   min x y = if x <= y then x else y
--
-- The class-method dispatcher binds @min@ / @max@ on demand via the
-- env-fallback's @tryClassMethodFromRegistry@ (same path as the
-- 93eee1a @compare@ removal). Both default bodies bottom out in the
-- still-builtin @<=@ dispatcher (slot 1 of @ordDispatch@), which has
-- primitive Int / Char / Double / String fast paths plus user
-- @Ord@-instance lookup plus structural VCon fallback.
--
-- Lock down the cases the source path covers:
--
--   * primitive Int / Char / Double compares (fast paths in @ordCmp@),
--   * @[Char]@ string compare (recurses into Char element compare via
--     the @Ord [a]@ source instance @compare (x:xs) (y:ys) = ...@),
--   * @deriving Ord@ on a sum type (declaration-index ordering via
--     @ordDispatch@'s structural fallback driven through @<=@),
--   * @deriving Ord@ on a product type (lexicographic field compare),
--   * a user @instance Ord@ providing @<=@ explicitly — the source
--     @min@ / @max@ default body must dispatch through that user
--     @<=@, so an inverted instance produces inverted picks,
--   * higher-order use via @foldr1 min@ / @foldr1 max@ to verify
--     @min@ / @max@ resolve as values, not just at syntactic call
--     sites.

data Color = Red | Green | Blue deriving (Eq, Ord, Show)
data Pair  = Pair Int Int        deriving (Eq, Ord, Show)

-- User-defined Ord that inverts the natural Int order.  If @min@ /
-- @max@ resolve through the source @class Ord@ default
-- (@if x <= y then ...@) and our @<=@ override is honoured, then
-- @min@ on @Inv@ values returns the *larger* underlying Int and
-- @max@ returns the smaller.
data Inv = Inv Int deriving (Eq, Show)
instance Ord Inv where
    Inv a <= Inv b = a >= b

main :: IO ()
main = do
    -- Primitives.
    print (min (3 :: Int) 7)            -- 3
    print (max (3 :: Int) 7)            -- 7
    print (min (5 :: Int) 5)            -- 5
    print (min 'a' 'b')                 -- 'a'
    print (max 'a' 'b')                 -- 'b'
    print (min (1.5 :: Double) 2.5)     -- 1.5
    print (max (1.5 :: Double) 2.5)     -- 2.5
    -- String compare via Ord [Char] recursion.
    print (min "abc" "abd")             -- "abc"
    print (max "abc" "abd")             -- "abd"
    print (min "z"   "a")               -- "a"
    -- Derived Ord on sum / product types.
    print (min Red Blue)                -- Red
    print (max Red Blue)                -- Blue
    print (min (Pair 1 2) (Pair 1 3))   -- Pair 1 2
    print (max (Pair 2 0) (Pair 1 99))  -- Pair 2 0
    -- User instance: inverted <=, so min returns the LARGER Int.
    print (min (Inv 3) (Inv 7))         -- Inv 7
    print (max (Inv 3) (Inv 7))         -- Inv 3
    -- Higher-order: foldr1 with min / max as the binop.
    print (foldr1 min [3, 1, 4, 1, 5, 9, 2, 6 :: Int])  -- 1
    print (foldr1 max [3, 1, 4, 1, 5, 9, 2, 6 :: Int])  -- 9
