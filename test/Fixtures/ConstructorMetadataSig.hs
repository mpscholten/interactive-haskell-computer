{-# LANGUAGE GADTs #-}
module ConstructorMetadataSig where

data G a where
    Mk :: b -> G [b]
