module CallerB (observe) where

import Data.IORef
import Provider (sharedRef)

observe :: IO Int
observe = readIORef sharedRef
