-- A.1 — Haskell Report §3.17.2 + GHC BangPatterns: `!x <- m` must
-- force the bound result to WHNF before the rest of the do-block
-- runs.  Detection via an IORef bumped from inside the thunk
-- returned by the monadic action.
module Main where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE markerRef #-}
markerRef :: IORef Int
markerRef = unsafePerformIO (newIORef 0)

bumpMarker :: a -> a
bumpMarker x = unsafePerformIO $ do
    modifyIORef markerRef (+1)
    pure x

main :: IO ()
main = do
    !_x <- pure (bumpMarker (42 :: Int))
    n <- readIORef markerRef
    if n == 0 then putStrLn "not forced" else putStrLn "forced"
