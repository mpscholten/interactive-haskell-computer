fizzbuzz :: Int -> String
fizzbuzz n
    | mod n 15 == 0 = "FizzBuzz"
    | mod n 3  == 0 = "Fizz"
    | mod n 5  == 0 = "Buzz"
    | otherwise     = show n

loop :: Int -> IO ()
loop n =
    if n > 100
        then pure ()
        else do
            putStrLn (fizzbuzz n)
            loop (n + 1)

main :: IO ()
main = loop 1
