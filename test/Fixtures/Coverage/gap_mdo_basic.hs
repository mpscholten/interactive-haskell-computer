-- Gap: RecursiveDo `mdo` keyword not recognised. Ref: HsExtMisc.hs.
{-# LANGUAGE RecursiveDo #-}
main = do
  r <- mdo
    x <- return (1 :: Int)
    return x
  print r
