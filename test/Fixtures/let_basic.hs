-- let-in: x is bound to 10, used twice in the body.
main = print (let x = 10 in x * x + x)
-- 100 + 10 = 110
