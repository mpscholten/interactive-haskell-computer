-- 3-arg function: fused multiply-add.
fma :: Int -> Int -> Int -> Int
fma a b c = a * b + c

-- 4-arg function: weighted sum.
ws :: Int -> Int -> Int -> Int -> Int
ws a b c d = a + 2*b + 3*c + 4*d

main = do
    print (fma 3 4 5)         -- 17
    print (ws 1 1 1 1)        -- 10
    print (ws 10 20 30 40)    -- 10 + 40 + 90 + 160 = 300
