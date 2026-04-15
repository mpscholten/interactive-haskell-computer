main = do
    a <- return 1
    b <- return 2
    print (a + b)
-- expects:
--   3
