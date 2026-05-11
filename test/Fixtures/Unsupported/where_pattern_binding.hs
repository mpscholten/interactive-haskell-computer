-- Gap: pattern binding in a where clause.
--
-- Same shape works in a let-expression but fails as a where-clause:
--
--     f bs = let BS _ m = bs in m + 1      -- OK, prints 6
--     f bs = m + 1 where BS _ m = bs       -- prints "<function>"
--
-- From a 2026-05-11 investigation traced via [pc] @parseClause@:
-- only the entry @main@ clause goes through @parseClause@; @f@'s
-- own clause is parsed elsewhere on a path that doesn't desugar
-- the where-clause pattern binding.  @f@ evaluates to a function-
-- shaped value rather than the expected Int.
--
-- Hits real Hackage code: 'Data.ByteString.Internal.Type.concat'
-- uses
--   goLen1 bss0 bs (BS _ len:bss) = goLen bss0 (len' + len) bss
--     where BS _ len' = bs
-- so the 'Data.ByteString.concat' shim (rule-4 violation) can't
-- be removed until this gap is fixed.  See the inline comment at
-- the @Data.ByteString.concat@ shim registration in 'IHC.Builtins'.
module Main where

data MyBS = BS Int Int

f :: MyBS -> Int
f bs = m + 1
  where BS _ m = bs

main :: IO ()
main = print (f (BS 0 5))
