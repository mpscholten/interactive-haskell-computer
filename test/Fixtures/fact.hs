-- Classic recursive factorial.
fact n = if n <= 1 then 1 else n * fact (n - 1)
main = fact 10
-- 10! = 3628800
