import Prelude

class Wrap m where
    wrap :: a -> m a

instance Wrap Maybe where
    wrap = Just

main :: IO Int
main = pure (case (wrap 23 :: Maybe Int) of
    Just value -> value
    Nothing -> 0)
