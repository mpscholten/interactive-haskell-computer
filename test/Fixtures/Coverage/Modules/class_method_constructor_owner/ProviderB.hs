{-# LANGUAGE GADTs #-}
module ProviderB (Wrap(..), pickB) where

data Wrap a where
    Wrap :: Bool -> Wrap Bool

class PickB a where
    pickB :: Wrap a -> Int

instance PickB Int where
    pickB _ = 32

instance PickB Bool where
    pickB _ = 42
