-- let-bound name shadows nothing; references a function param.
hyp a b = let asq = a * a in let bsq = b * b in asq + bsq
main = print (hyp 3 4)
-- 9 + 16 = 25
