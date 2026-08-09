{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Typeable (Typeable)
import GHC.Internal.Exception.Type (Exception(..), SomeException(..))

data Custom = Custom deriving (Show, Typeable)

instance Exception Custom where
    toException e = SomeException e
    fromException _ = Just Custom

main :: IO ()
main = do
    case toException Custom of
        SomeException _ -> putStrLn "source toException"
    case fromException (SomeException Custom) of
        Just Custom -> putStrLn "source fromException"
        Nothing -> putStrLn "wrong instance"
