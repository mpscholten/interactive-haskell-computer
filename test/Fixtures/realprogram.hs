-- A "real Haskell" style program using everything we have.
fact :: Int -> Int
fact n = if n <= 1 then 1 else n * fact (n - 1)

fib :: Int -> Int
fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)

isPrime' :: Int -> Int -> Int
isPrime' n d =
    if d * d > n
        then 1
        else if mod n d == 0
                 then 0
                 else isPrime' n (d + 1)

isPrime :: Int -> Int
isPrime n = if n <= 1 then 0 else isPrime' n 2

main :: IO ()
main = do
    putStrLn "=== math demo ==="
    putStrLn "fact 10 ="
    print (fact 10)
    putStrLn "fib 20 ="
    print (fib 20)
    putStrLn "is 97 prime?"
    print (isPrime 97)
    putStrLn "is 100 prime?"
    print (isPrime 100)
    putStrLn "done."
