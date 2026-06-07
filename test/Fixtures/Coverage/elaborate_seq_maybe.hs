main = do
    print ((Just 1 >> Just 2) :: Maybe Int)
    print ((Nothing >> Just 2) :: Maybe Int)
