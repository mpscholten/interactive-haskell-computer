{-# LANGUAGE TypeApplications #-}
-- Visible type application on class methods (`pure @Parser`, `empty @Parser`).
class Empty f where
    empty' :: f a

instance Empty Maybe where
    empty' = Nothing

instance Empty [] where
    empty' = []

main = do
    print (pure @Maybe (1 :: Int))
    print (fmap @Maybe (+1) (Just (1 :: Int)))
    print (empty' @Maybe :: Maybe Int)
    print (empty' @[] :: [Int])
