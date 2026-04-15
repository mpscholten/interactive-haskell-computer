-- All six relops + && and ||.
allTrue =
       (1 == 1) * (1 /= 2)
     * (1 <  2) * (2 >  1)
     * (1 <= 1) * (1 >= 1)

main = print allTrue
-- 1
