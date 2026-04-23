main = print $ do
    x <- Just 3
    pure (x + 1) :: Maybe Int
