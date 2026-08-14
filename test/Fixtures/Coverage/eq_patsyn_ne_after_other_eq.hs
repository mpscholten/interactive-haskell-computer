{-# LANGUAGE PatternSynonyms #-}
-- Combined leftover: first Eq on one newtype+patsyn, then (/=) on a
-- second newtype+patsyn.  == is GREEN; the class default
--   x /= y = not (x == y)
-- plus the sibling default
--   x == y = not (x /= y)
-- loops when the second tag is an unexpanded nullary patsyn, or a
-- /= miss drains the rest of the Eq catalogue.  Structural: two
-- unary newtypes, bidirectional patterns.  No host-library names.
newtype WrapA = WrapA Int deriving Eq
newtype WrapB = WrapB Int deriving Eq

pattern A1 = WrapA 1
pattern A2 = WrapA 2
pattern B1 = WrapB 1
pattern B2 = WrapB 2

main = do
    print (A1 == A2)
    print (B1 /= B2)
    print (B1 /= B1)
    print (A1 /= A2)
