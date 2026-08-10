{-# LANGUAGE GADTs #-}
module Main where

import qualified ProviderA
import qualified ProviderB

main :: Int
main = ProviderA.pickA (ProviderA.Wrap 1)
     + ProviderB.pickB (ProviderB.Wrap False)
