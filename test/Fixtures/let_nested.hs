-- Nested let, with the inner let referencing the outer.
main = print (let a = 5 in let b = a + 1 in a * b)
-- 5 * 6 = 30
