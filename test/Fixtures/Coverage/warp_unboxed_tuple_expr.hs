{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}

-- Regression guard: unboxed tuple at expression position.
--
-- Pattern-side parser (parseUnboxedTuplePat) already worked. This locks
-- in the symmetric expression-side path: TkLUnbox starts an atom and
-- `parseUnboxedTuple` builds an EApp chain over a `(#,#)`-shaped ctor.
-- Required to source-load GHC.Conc.Sync (warp-dryrun-findings.md #4).
--
-- Single-binding form because there's a separate pre-existing master
-- regression (post-bdc9cd4) where ANY program with two top-level value
-- bindings prints `IHC.Eval: unbound variable <first>`. That bug also
-- breaks `bang_pattern_strict` and `lazy_strict_app`. Once it's fixed,
-- a multi-binding variant of this fixture should be added.

main :: IO ()
main = case (# 10, 11 #) of
    (# a, b #) -> print (a + b)
