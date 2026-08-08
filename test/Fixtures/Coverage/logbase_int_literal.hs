-- Unannotated integer literals in Floating/Double context must still
-- work through D# pattern bridges (VInt → Double).
--
-- warp PackInt.packIntegral does:
--   n' = fromIntegral n + 1 :: Double
--   len = ceiling $ logBase 10 n'
-- Without D#/VInt matchPat, log 10 fails and / sees a function.
main :: IO ()
main = do
    print (log (10 :: Double))
    print (logBase (10 :: Double) (201 :: Double))
    -- Unannotated 10 (VInt) against Floating Double:
    print (logBase 10 (201 :: Double))
    print (ceiling (logBase 10 (201 :: Double)) :: Int)
    print (ceiling (logBase 10 (fromIntegral (200 :: Int) + 1 :: Double)) :: Int)
