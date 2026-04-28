-- C.1 — GADT pattern-match refinement (Haskell Report addendum /
-- GHC user guide §6.4.7).
--
-- A GADT-form data declaration like @data Equ a b where Refl ::
-- Equ a a@ uses an existential-style head where matching @Refl@
-- proves @a ~ b@; the body of the case-alt typechecks under the
-- refined substitution.  ihc is type-permissive — it parses GADT
-- syntax and runs the @coerce Refl x = x@ shape without any
-- refinement step because nothing at runtime cares about the type
-- equality.  The actual refinement-substitution work happens in
-- the elaborator and is gated on the typed-IR slice (C.2.3
-- follow-up + B.5b).
--
-- This fixture locks in current behaviour: the GADT-form
-- declaration parses cleanly, the @Refl@ constructor matches at
-- the runtime layer, and @coerce Refl x = x@ behaves like the
-- identity at runtime, regardless of the target type.
{-# LANGUAGE GADTs #-}
module Main where

data Equ a b where
    Refl :: Equ a a

coerce :: Equ a b -> a -> b
coerce Refl x = x

main :: IO ()
main = do
    print (coerce Refl (5 :: Int))
    print (coerce Refl 'a')
