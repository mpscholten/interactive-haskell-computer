{-# LANGUAGE CPP #-}
#ifdef __GLASGOW_HASKELL__
main = putStrLn "ghc-flavoured"
#else
main = putStrLn "other"
#endif
