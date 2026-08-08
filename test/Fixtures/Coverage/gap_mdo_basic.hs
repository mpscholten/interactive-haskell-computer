-- Gap (closed for keyword only): RecursiveDo `mdo` is accepted and parsed like
-- `do`. Full recursive-do desugaring / `rec` blocks are still incomplete.
-- Ref: HsExtMisc.hs.
{-# LANGUAGE RecursiveDo #-}
main = do
  r <- mdo
    x <- return (1 :: Int)
    return x
  print r
