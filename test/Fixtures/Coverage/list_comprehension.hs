-- List comprehensions
squares = [x * x | x <- [1..5 :: Int]]
evens20 = [x | x <- [1..20 :: Int], x `mod` 2 == 0]
pairs = [(x, y) | x <- [1..3 :: Int], y <- [1..3 :: Int], x < y]

main = do
    print squares
    print evens20
    print pairs
