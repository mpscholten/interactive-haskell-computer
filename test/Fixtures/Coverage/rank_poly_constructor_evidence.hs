{-# LANGUAGE RankNTypes #-}
import Prelude

class Choose m where
    choose :: a -> m a

instance Choose Maybe where
    choose = Just

newtype Packed a = Packed (forall m. Choose m => m a)

unpackMaybe :: Packed a -> Maybe a
unpackMaybe (Packed action) = action

main :: IO Int
main = pure (case unpackMaybe (Packed (choose 37)) of
    Just value -> value
    Nothing -> 0)
