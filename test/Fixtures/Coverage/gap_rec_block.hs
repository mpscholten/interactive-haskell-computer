-- RecursiveDo nested `rec` block inside do-notation.
-- Minimal (non-mfix) desugar: rec body stmts are spliced into the enclosing
-- do. Sufficient for non-recursive rec bodies.
-- Ref: HsExtMisc.hs RecursiveDo.
{-# LANGUAGE RecursiveDo #-}
main = do
  r <- do
    rec
      x <- return (1 :: Int)
    return x
  print r
