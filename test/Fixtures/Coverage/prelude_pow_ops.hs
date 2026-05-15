-- Builtins-removal: the power operators must resolve via library
-- source, not the historical @powOpB@ / @powFloatOpB@ shims.
--
--   * (^)  :: (Num a, Integral b)        => a -> b -> a
--   * (^^) :: (Fractional a, Integral b) => a -> b -> a
--     Both are *top-level functions* in
--     @~/.cache/ihc/sources/ghc-internal-9.1003.0/src/GHC/Internal/Real.hs@
--     (lines 744 / 772).  They recurse via @(*)@, @`quot`@, @recip@,
--     @negate@ (all already graduated) and resolve through the
--     env-fallback EVar path — no class-method seed needed.
--
--   * (**) :: Floating a => a -> a -> a
--     This *is* a @class Floating@ method.  The @Floating Double@
--     instance body @(**) x y = powerDouble x y@
--     (@GHC/Internal/Float.hs:756@) bottoms out on
--     @powerDouble (D# x) (D# y) = D# (x **## y)@ (Float.hs:1594).
--     Seeded @("**","Floating")@ in
--     @TypeGlobals.seedBuiltinClassMethodSigs@ and routed via
--     @tryClassMethodFromRegistry@ → @classMethodDispatcher@.  The
--     @**##@ Double# power primop is a GHC.Prim intrinsic (no .hs
--     source) registered as a carve-out in @IHC.Builtins@.
module Main where

main :: IO ()
main = do
    -- (^): Int base, Int exponent, repeated multiplication
    print (2 ^ (10 :: Int))
    -- (^): zero exponent edge case
    print (3 ^ (0 :: Int))
    -- (^^): Fractional base, negative Int exponent (recip path)
    print (2.0 ^^ (-3 :: Int))
    -- (^^): non-negative exponent path
    print (2.0 ^^ (3 :: Int))
    -- (**): Floating class method on Double
    print (2.0 ** 0.5 :: Double)
    print (9.0 ** 0.5 :: Double)
