{-# LANGUAGE GADTs #-}
module ProviderA (Wrap(..), pickA) where

data Wrap a where
    Wrap :: a -> Wrap a

class PickA a where
    pickA :: Wrap a -> Int

instance PickA Int where
    pickA _ = 31

instance PickA Bool where
    pickA _ = 41
