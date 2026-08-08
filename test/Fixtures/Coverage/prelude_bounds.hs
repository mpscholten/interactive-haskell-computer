-- Builtins-removal: 'minBound' / 'maxBound' must resolve through the
-- source-loaded Bounded class path, not the historical host Int builtin.
--
-- The Int cases force the explicit core GHC.Internal.Enum Bounded
-- instance via type annotation. Bool/Ordering exercise source
-- standalone-derived core instances. The local enum keeps coverage
-- for derived Bounded synthesis on nullary user data.
module Main where

data Color = Red | Green | Blue
    deriving (Show, Eq, Ord, Enum, Bounded)

main :: IO ()
main = do
    print (minBound :: Int)
    print (maxBound :: Int)
    print (minBound :: Bool)
    print (maxBound :: Bool)
    print (minBound :: Ordering)
    print (maxBound :: Ordering)
    print (minBound :: Color)
    print (maxBound :: Color)
