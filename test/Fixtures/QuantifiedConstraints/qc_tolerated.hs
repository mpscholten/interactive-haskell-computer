-- B.5a — quantified constraints (GHC user guide §6.8.10).
--
-- @class (forall a. Eq a => Eq (f a)) => Eq1 f where ...@ declares
-- a constraint that quantifies over a fresh type variable.  In GHC
-- the elaborator skolemises @a@, discharges the body's predicates,
-- and re-generalises; ihc's elaborator is rank-1 only and doesn't
-- run a quantified-constraint solver, so the @forall@ clause has
-- no semantic effect today.
--
-- 'parseClassHead' / 'skipFundep' handle the depth-aware token
-- scan correctly: the @forall@ is treated as part of the class
-- context, ConIds inside the parens get captured as superclass
-- names (currently both inner @Eq@ ConIds register; that's not
-- quite the right shape but doesn't break dispatch since the
-- superclass map isn't consulted for dispatch yet — B.1 shipped
-- as a data layer only).
--
-- This fixture locks in two things:
--   * Programs that use @QuantifiedConstraints@ still load and run.
--   * Class methods on @Eq1@-style classes dispatch correctly via
--     the head type (here: @Box@), since the actual operational
--     mechanism — runtime tag-keyed dispatch — is unaffected by
--     the type-level quantifier.
--
-- Real quantified-constraint solving (B.5b) ships after the
-- elaborator-integrated lowering (C.2.3 follow-up) is in place.
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

class (forall a. Eq a => Eq (f a)) => Eq1 f where
    eq1 :: Eq a => f a -> f a -> Bool

data Box a = Box a

instance Eq a => Eq (Box a) where
    Box x == Box y = x == y

instance Eq1 Box where
    eq1 (Box x) (Box y) = x == y

main :: IO ()
main = do
    print (eq1 (Box (1 :: Int)) (Box 1))
    print (eq1 (Box 'a') (Box 'b'))
