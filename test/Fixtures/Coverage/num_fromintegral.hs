-- Builtins-removal: 'fromIntegral' source-loads from
-- GHC.Internal.Real as @fromInteger . toInteger@.
--
-- This covers the common Int -> Double result-annotation path.

main = do
    print (fromIntegral (42 :: Int) :: Double)
