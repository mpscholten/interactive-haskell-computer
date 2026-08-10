module ConstructorMetadataH98Positive where

data Many a = Empty | Strict !a | Unpacked {-# UNPACK #-} !Int | Wrapped (Maybe a) | Listed [a] | Qualified Data.Int.Int
newtype Single a = Single (Maybe a)
