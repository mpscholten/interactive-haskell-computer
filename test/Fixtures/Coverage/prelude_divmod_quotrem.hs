-- Builtins-removal: divMod / quotRem must resolve via the
-- source-loaded @Integral Int@ instance in GHC.Internal.Real,
-- not the historical @divModB@ / @quotRemB@ shims.
--
-- The instance bodies (Real.hs:471-482) route through
--   * @a `quotRem` b@ → 'quotRemInt'  (Base.hs:2428)
--       → 'quotRemInt#' primop (registered)
--   * @a `divMod`  b@ → 'divModInt'   (Base.hs:2444)
--       → 'divModInt#', itself source-loaded from ghc-prim's
--         GHC/Classes.hs:840, riding 'quotRemInt#' plus the
--         'notI#' GHC.Prim primop carve-out.
--
-- quotRem truncates toward zero; divMod truncates toward
-- negative infinity.  They agree for like-signed operands and
-- diverge for mixed signs — exactly the cases checked below.
module Main where

main :: IO ()
main = do
    -- Positive / positive: divMod and quotRem agree.
    print (divMod  17 5 :: (Int, Int))
    print (quotRem 17 5 :: (Int, Int))
    -- Negative numerator: divMod rounds toward -inf (remainder
    -- takes the sign of the divisor); quotRem rounds toward zero
    -- (remainder takes the sign of the dividend).
    print (divMod  (-17) 5 :: (Int, Int))
    print (quotRem (-17) 5 :: (Int, Int))
    -- Negative denominator.
    print (divMod  17 (-5) :: (Int, Int))
    print (quotRem 17 (-5) :: (Int, Int))
    -- Both negative.
    print (divMod  (-17) (-5) :: (Int, Int))
    print (quotRem (-17) (-5) :: (Int, Int))
    -- Identity: divisor 1.
    print (divMod  42 1 :: (Int, Int))
    print (quotRem 42 1 :: (Int, Int))
    -- Tuple destructuring still works through the dispatcher.
    case divMod 23 4 of
        (d, m) -> print (d, m)
    case quotRem 23 4 of
        (q, r) -> print (q, r)
