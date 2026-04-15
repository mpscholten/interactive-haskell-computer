-- Type signatures are parsed and ignored.
fact :: Int -> Int
fact n = if n <= 1 then 1 else n * fact (n - 1)

main :: IO ()
main = print (fact 6)
-- 720
