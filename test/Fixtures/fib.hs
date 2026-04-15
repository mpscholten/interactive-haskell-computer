-- Naive Fibonacci — exercises self-recursion + double recursive calls
-- (which require pushing/popping x0 across the second call).
fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
main = fib 10
-- fib 10 = 55
