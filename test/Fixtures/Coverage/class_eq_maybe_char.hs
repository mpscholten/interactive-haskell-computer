eqMaybe Nothing  Nothing  = True
eqMaybe (Just x) (Just y) = x == y
eqMaybe _        _        = False

main = do
    print (eqMaybe (Just 'a') (Just 'a'))
    print (eqMaybe (Just 'a') (Just 'b'))
    print (eqMaybe Nothing (Nothing :: Maybe Char))
    print (eqMaybe (Just 'x') Nothing)
