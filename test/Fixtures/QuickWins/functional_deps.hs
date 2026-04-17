{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Exercises parser support for 'class C a b | a -> b where ...'.
-- The fundep has no runtime effect in IHC — it's purely a type-checker
-- hint in GHC — so once the scanner stops tripping on '|' the class
-- head parses like any other MPTC and dispatch goes through the
-- single-key fast path.
class Collection c e | c -> e where
    unitColl :: c -> e -> c
instance Collection [Int] Int where
    unitColl _ x = [x]
main = print (unitColl ([] :: [Int]) (1 :: Int))
