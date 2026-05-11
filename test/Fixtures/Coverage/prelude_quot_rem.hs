-- Builtins-removal: quot / rem must resolve via the source-loaded
-- Prelude re-export (Integral Int instance in GHC.Internal.Real),
-- not the historical @binOpInt quot@ / @binOpInt rem@ shims.
-- The instance bodies route through quotInt / remInt (Haskell wrappers
-- in GHC.Internal.Base) and bottom on the quotInt# / remInt# primops.
--
-- Compared with div / mod (truncated-toward-negative-infinity), quot
-- rounds toward zero and rem follows quot to satisfy
--   (a `quot` b) * b + (a `rem` b) == a
module Main where

main :: IO ()
main = do
    -- Positive / positive: quot and rem match div and mod.
    print (quot 17 5  :: Int)
    print (rem  17 5  :: Int)
    -- Negative numerator: quot rounds toward zero; div rounds down.
    print (quot (-17) 5  :: Int)
    print (rem  (-17) 5  :: Int)
    -- Negative denominator.
    print (quot 17 (-5)  :: Int)
    print (rem  17 (-5)  :: Int)
    -- Both negative.
    print (quot (-17) (-5) :: Int)
    print (rem  (-17) (-5) :: Int)
    -- Identity: a `quot` 1 == a, a `rem` 1 == 0.
    print (quot 42 1  :: Int)
    print (rem  42 1  :: Int)
