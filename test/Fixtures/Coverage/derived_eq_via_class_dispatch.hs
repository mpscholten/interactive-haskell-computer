-- Locks down the source-loaded @derived Eq@ synthesis added by
-- 'IHC.Scheduler.registerDerivedEqInstances' for the in-line
-- @data ... deriving Eq@ shape.
--
-- A polymorphic @eqIt :: Eq a => a -> a -> Bool@ forces the
-- elaborator to emit @ETypedMethod \"Eq\" \"==\" tag@ rather than the
-- bare @EVar \"==\"@ that resolves to the @eqDispatch@ builtin
-- shim — so the synthesised structural body actually fires for
-- @MkPt 1 2 == MkPt 1 2@ and friends instead of being shadowed by
-- the host @eqVals@ inline.
--
-- The synthesised body is mechanically equivalent to what GHC emits:
--
--   (==) (MkPt x1 y1) (MkPt x2 y2) = x1 == x2 && y1 == y2
--
-- with the per-field @==@ recursing through the class-method
-- dispatcher (Eq Int from @GHC.Classes@ → @eqInt@ → primop).
--
-- Same shape exercised by 'derived_eq_sum_type' (sum) and
-- 'derived_eq_recursive' (Tree) below.
data Pt = MkPt Int Int deriving Eq

eqIt :: Eq a => a -> a -> Bool
eqIt x y = x == y
{-# NOINLINE eqIt #-}

main :: IO ()
main = do
    print (eqIt (MkPt 1 2) (MkPt 1 2))   -- True
    print (eqIt (MkPt 1 2) (MkPt 1 3))   -- False
    print (eqIt (MkPt 1 2) (MkPt 3 2))   -- False
