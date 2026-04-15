-- Eq on Maybe via VCon structural dispatch.
main = do
    print (Just 1 == Just 1)
    print (Just 1 == Just 2)
    print (Nothing == Nothing)
    print (Just 3 == Nothing)
