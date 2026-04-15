-- Trial division using mod, /=, &&.
isPrime' n d =
    if d * d > n
        then True
        else if mod n d == 0
                 then False
                 else isPrime' n (d + 1)

isPrime n = if n <= 1 then False else isPrime' n 2

main = do
    print (isPrime 97)
    print (isPrime 100)
