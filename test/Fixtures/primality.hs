-- Trial division using mod, /=, &&.
isPrime' n d =
    if d * d > n
        then 1
        else if mod n d == 0
                 then 0
                 else isPrime' n (d + 1)

isPrime n = if n <= 1 then 0 else isPrime' n 2

main = print (isPrime 97)
-- 1
