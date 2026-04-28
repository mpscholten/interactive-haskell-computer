-- B.4 — Functional dependencies (GHC user guide §6.8.8).
--
-- A @class C a b | a -> b where ...@ declaration declares that @a@
-- determines @b@.  In GHC this lets the type checker improve
-- constraints (knowing two pending @C T S1@ and @C T S2@ unifies
-- @S1 ~ S2@); ihc's elaborator is rank-1 and doesn't run a full
-- constraint solver, so the FunDep clause has no runtime effect.
-- 'parseClassHead' / 'skipFundep' tolerate the @|@ clause cleanly,
-- and dispatch on the head type works through the existing
-- single-key path.  Real FunDep-driven constraint improvement
-- depends on the typed-IR slice (C.2).
--
-- This fixture is a regression guard for parser tolerance + the
-- single-arg dispatch path: a FunDep-using class still loads, the
-- instance still registers, and method calls still resolve.
module Main where

class Convertible a b | a -> b where
    convert :: a -> b

instance Convertible Int Bool where
    convert n = n > 0

main :: IO ()
main = do
    print (convert (5 :: Int))
    print (convert (0 :: Int))
