-- NumericUnderscores: underscores inside a numeric literal are separators.
-- 1_000_000 == 1000000; 0x_DEAD_BEEF == 0xDEADBEEF.
main = do
    print 1_000_000
    print 0xDEAD_BEEF
    print 1_000_000_000_000
