module CallerA (increment) where

import Data.IORef
import Provider (sharedRef)

increment :: IO ()
increment = modifyIORef sharedRef (+ 1)
