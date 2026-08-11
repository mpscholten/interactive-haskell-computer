{-# LANGUAGE RankNTypes #-}
module RankNRecord where

class Choose m where
    choose :: a -> m a

data Packed a = Packed
    { runPacked :: forall m. Choose m => m a
    }
