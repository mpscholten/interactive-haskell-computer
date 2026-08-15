{-# LANGUAGE MagicHash, UnboxedSums, UnboxedTuples #-}
-- HashMap.lookup# (knownElements in IHP.HSX.QQ.compileToHaskell) is:
--   lookup# :: (Eq k, Hashable k) => k -> HashMap k v -> (# (# #) | v #)
-- findEqOrBarOnLine treated the unboxed-sum `|` as a clause guard and
-- dropped the binding (unbound `lookup#` / unresolved same-module).
lookup# :: Int -> (# (# #) | Int #)
lookup# x = (# | x #)
main = case lookup# 41 of
    (# | n #) -> print (n + 1)
    (# (# #) | #) -> print (0 :: Int)
