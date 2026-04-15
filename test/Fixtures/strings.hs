main = do
    putStrLn ("Hello, " ++ "world!" ++ " The answer is " ++ show 42)
    putStrLn ("fib 10 = " ++ show (fib 10))

fib :: Int -> Int
fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
