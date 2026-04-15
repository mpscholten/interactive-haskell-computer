-- Combine relops with && and ||.
inRange x = (1 <= x) && (x <= 10)
either' x = (x == 0) || (x == 100)

main = do
    print (inRange 5)
    print (inRange 11)
    print (either' 0)
    print (either' 50)
