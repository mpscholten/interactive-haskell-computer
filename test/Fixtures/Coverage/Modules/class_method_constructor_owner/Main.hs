{-# LANGUAGE GADTs #-}
module Main where

import qualified ProviderA
import qualified ProviderB

main :: IO ()
main = do
    print (ProviderA.pickA (ProviderA.Wrap 1))
    print (ProviderB.pickB (ProviderB.Wrap False))
