-- Gap: Bang-strict do-bind (`!x <- action`). Seen in: conduit-1.3.6.1/Conduit/Internal/Conduit.hs. Ref: hackage-parser-gaps.md (conduit bucket 3).
{-# LANGUAGE BangPatterns #-}

import Data.IORef

main = do
    ref <- newIORef (41 :: Int)
    !x  <- readIORef ref
    print (x + 1)
