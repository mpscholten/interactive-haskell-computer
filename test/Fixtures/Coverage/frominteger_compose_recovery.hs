-- Recovery: Num.fromInteger applied to an unapplied toInteger-shaped
-- function should compose, not PatternMatchFail on IS|IP|IN.
--
-- Models the warp request-path failure where
--   fromInteger (toInteger …)
-- received a *function* (unapplied Integral method / left-associated
-- composition) instead of an Integer.
{-# LANGUAGE TypeApplications #-}
import Data.Word (Word8)

main :: IO ()
main = do
    -- Healthy path (control)
    print (fromIntegral (5 :: Word8) :: Int)
    -- TypeApplications on toInteger then fromIntegral-style use
    print (fromInteger (toInteger @Word8 7) :: Int)
    -- Explicit compose of class methods
    print ((fromInteger . toInteger) (9 :: Word8) :: Int)
