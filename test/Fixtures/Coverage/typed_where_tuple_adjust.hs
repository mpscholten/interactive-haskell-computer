{-# LANGUAGE BangPatterns #-}
-- epochTimeToHTTPDate / toYYMMDD are signed
--   toYYMMDD :: Int -> (Int,Int,Int)
-- with a where-clause that calls a signed recursive helper (adjust).
-- Eval-time ExpectType on that constraint-free scheme walked the
-- where-clause and diverged (withDateCache hang).  CTime 0 took the
-- first adjust guard; a real epoch (20679 days) never returned.

adjust :: Int -> Int -> Int -> (Int, Int)
adjust !ty td aj
  | td >= aj  = (ty, td - aj)
  | otherwise = adjust (ty - 1) (td + 365) aj

toYYMMDD :: Int -> (Int, Int, Int)
toYYMMDD x = (yy, 1, days)
  where
    (y, d) = x `quotRem` 365
    cy = 1970 + y
    cy' = cy - 1
    leap = cy' `quot` 4 - cy' `quot` 100 + cy' `quot` 400 - 477
    (yy, days) = adjust cy d leap

main :: IO ()
main = do
    print (toYYMMDD 0)
    print (toYYMMDD 20679)
