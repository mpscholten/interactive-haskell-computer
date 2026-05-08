{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

-- Regression guard: full unboxed-tuple syntax round-trip.
--
-- Exercises three positions for the `(# … #)` syntax:
--   * Type position:  `(# Int#, Int# #) -> Int#` in the signature.
--   * Pattern:        `addPair (# a, b #) = …`
--   * Expression:     `addPair (# 10, 11 #)`
--
-- The interpreter has no real unboxed heap; unboxed tuples desugar to
-- ordinary `VCon "(#,#)" [...]` (see `parseUnboxedTuple` /
-- `parseUnboxedTuplePat`).  Top-level signatures are skipped at the
-- scan layer ('scanAllTopLevelNames' only enumerates lines with an
-- '=' or guard), so the @(# … #)@ shape in the signature passes
-- through transparently — what matters is the expression and pattern
-- parsers (@startsAtom TkLUnbox@, @startsPat TkLUnbox@).
--
-- Required by warp-dryrun-findings.md (blocker #4): GHC.Conc.Sync uses
-- unboxed tuples extensively, e.g.
-- `unpackCString# :: Addr# -> (# Int#, ByteArray# #)`.
--
-- Companion to `warp_unboxed_tuple_expr.hs`, which only covers the
-- expression-side path; this one additionally pins the type-sig and
-- function-pattern paths so the next regression in any of them lights
-- up here.

import GHC.Exts (Int#, (+#))

addPair :: (# Int#, Int# #) -> Int#
addPair (# a, b #) = a +# b

main :: IO ()
main = print (addPair (# 10, 11 #))
