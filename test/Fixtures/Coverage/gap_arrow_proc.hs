-- Gap: Arrow notation `proc` / `-<` not parsed. Ref: HsExtMisc.hs.
{-# LANGUAGE Arrows #-}
import Control.Arrow (arr, returnA)

main = print ((proc x -> do
  y <- arr id -< x
  returnA -< y) (42 :: Int))
