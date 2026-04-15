-- Phase 2.8: bit ops and quot/rem (simpler than raw primops).
-- Tests the new builtins that are needed by containers.
main :: IO ()
main = do
    -- Bit operations
    let x = 12 :: Int   -- 0b1100
        y = 10 :: Int   -- 0b1010
    print (x .&. y)        -- 8
    print (x .|. y)        -- 14
    print (xor x y)        -- 6
    print (shiftL 1 3)     -- 8
    print (shiftR 16 2)    -- 4
