-- Combine relops with && and ||.
inRange x = (1 <= x) && (x <= 10)
either x  = (x == 0) || (x == 100)

main = print (inRange 5 + inRange 11 + either 0 + either 50)
-- 1 + 0 + 1 + 0 = 2
