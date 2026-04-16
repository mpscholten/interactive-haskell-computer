-- Mixed Int/Double arithmetic with fromIntegral
average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

main = do
    let xs = [1, 2, 3, 4, 5] :: [Int]
    print (fromIntegral (42 :: Int) :: Double)
    print (sum xs)
    print (length xs)
