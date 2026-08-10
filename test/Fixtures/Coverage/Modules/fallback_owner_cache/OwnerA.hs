module OwnerA (fromA) where

import ProviderA (shared)

fromA :: Int
fromA = shared
