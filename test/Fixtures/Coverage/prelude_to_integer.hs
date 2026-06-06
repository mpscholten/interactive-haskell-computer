-- Builtins-removal: 'toInteger' and 'fromIntegral' must both resolve
-- through source-loaded class methods.
--
-- 'toInteger' uses the source-loaded Integral Int instance
-- (GHC.Internal.Real:442):
--
--   toInteger (I# i) = IS i
--
-- 'fromIntegral' is now source-loaded from GHC.Internal.Real:
--
--   fromIntegral = fromInteger . toInteger
--
-- This fixture verifies the end-to-end round-trip.  The Integer
-- produced by 'toInteger' can arrive as ghc-bignum's 'IS' constructor;
-- 'IHC.Classes.typeTagOf' normalizes IS/IP/IN to the Integer dispatch
-- tag so source-loaded 'Integral Integer.toInteger' handles the input
-- side of 'fromIntegral'.
module Main where

main :: IO ()
main = do
    -- Round-trip: Int -> Integer -> Int through source-loaded
    -- Integral.toInteger and source-loaded fromIntegral.
    print (fromIntegral (toInteger (0    :: Int)) :: Int)
    print (fromIntegral (toInteger (5    :: Int)) :: Int)
    print (fromIntegral (toInteger (-7   :: Int)) :: Int)
    print (fromIntegral (toInteger (1000 :: Int)) :: Int)
    print (fromIntegral (toInteger ((-100000) :: Int)) :: Int)
