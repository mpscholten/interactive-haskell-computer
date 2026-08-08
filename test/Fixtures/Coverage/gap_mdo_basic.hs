-- RecursiveDo `mdo` is accepted and parsed like `do` (non-recursive desugar).
-- Nested `rec` blocks: see gap_rec_block.hs. Full mfix knot-tying is incomplete.
-- Ref: HsExtMisc.hs.
{-# LANGUAGE RecursiveDo #-}
main = do
  r <- mdo
    x <- return (1 :: Int)
    return x
  print r
