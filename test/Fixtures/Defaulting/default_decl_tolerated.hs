-- A.4 — Haskell Report §4.3.4 numeric defaulting.
--
-- A top-level @default (T1, T2, ...)@ declaration overrides the
-- standard @(Integer, Double)@ default list when an ambiguous numeric
-- class constraint reaches the binding's top level.  ihc currently
-- monomorphises every integer literal at parse time (A.3 minimum
-- scope: out-of-Int64 literals become 'LInteger', everything else
-- stays 'LInt'), so there's no ambiguous numeric constraint for
-- defaulting to act on — the rule applies vacuously.  This fixture
-- locks in the lexer/scanner's tolerance of the @default@ keyword
-- so the surrounding program runs cleanly; a follow-up slice (after
-- the elaborator-driven @fromInteger@-insertion path lands) will
-- replace it with a fixture that actually exercises the rule.
module Main where

default (Int, Double)

main :: IO ()
main = print (1 + 2)
