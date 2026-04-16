-- Phase 2.9.5: GADT constructor arity counting.
-- Pair2 has arity 2 (two fields before return type).
data Pair where
    Pair2 :: Int -> Int -> Pair

pairSum :: Pair -> Int
pairSum (Pair2 a b) = a + b

main :: IO ()
main = print (pairSum (Pair2 10 32))
