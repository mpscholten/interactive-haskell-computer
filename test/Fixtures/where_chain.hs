-- Each where-binding can reference earlier ones (sequential scope).
quadratic a b c x = result
  where
    ax2 = a * x * x
    bx  = b * x
    sum = ax2 + bx
    result = sum + c

main = do
    print (quadratic 1 (-3) 2 5)    -- 1*25 + (-3)*5 + 2 = 25 - 15 + 2 = 12
    print (quadratic 2 1 0 3)       -- 2*9 + 3 + 0 = 21
