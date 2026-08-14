{-# LANGUAGE PatternSynonyms #-}
-- Combined leftover: first Eq on one newtype+patsyn (Family-shaped),
-- then Prelude.elem on a second newtype+patsyn (SocketType-shaped).
-- Standalone elem was GREEN; after the first Eq the second use hung
-- (unexpanded patsyn alias → Eq catalogue drain / default == loop).
-- Structural: two unary newtypes, nullary bidirectional patterns, no
-- Network.Socket names in the interpreter.
newtype Fam = Fam Int deriving Eq
newtype Sty = Sty Int deriving Eq

pattern F1 = Fam 1
pattern F2 = Fam 2
pattern S1 = Sty 1
pattern S2 = Sty 2

main = do
    print (F1 == F2)
    print (S1 `elem` [S1, S2])
    print (S2 `elem` [S1])
