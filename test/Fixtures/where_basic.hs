-- where clause with two value bindings.
hyp a b = asq + bsq
  where
    asq = a * a
    bsq = b * b

main = print (hyp 3 4)
-- 25
