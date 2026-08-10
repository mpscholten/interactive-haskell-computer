module Provider (sharedRef) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

sharedRef :: IORef Int
sharedRef = unsafePerformIO (newIORef 0)
