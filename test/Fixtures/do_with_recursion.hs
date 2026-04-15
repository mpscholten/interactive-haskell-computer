fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)

main = do
    { putStrLn "Computing fibonacci numbers:"
    ; print (fib 5)
    ; print (fib 10)
    ; print (fib 15)
    }
-- expects:
--   Computing fibonacci numbers:
--   5
--   55
--   610
