{-# LANGUAGE RankNTypes #-}
import Prelude

class Choose m where
    choose :: a -> m a

instance Choose Maybe where
    choose = Just

data Packed a = Packed { runPacked :: forall m. Choose m => m a }

main :: IO Int
main = pure (case (runPacked (Packed (choose 41)) :: Maybe Int) of
    Just value -> value
    Nothing -> 0)
