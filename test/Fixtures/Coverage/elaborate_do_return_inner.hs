main = print $ do
    x <- Just 10
    return (x * 2) :: Maybe Int
