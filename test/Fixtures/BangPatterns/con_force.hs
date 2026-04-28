-- A.1 — Haskell Report §3.17.2: a bang sub-pattern in a constructor
-- pattern (`f (MkT !y) = …`) must force the field thunk to WHNF
-- before the body runs, even when the body discards y.  Detection
-- via an IORef bumped from inside a thunk passed as the field.
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

data T = MkT Int

f :: T -> Int
f (MkT !_y) = 0

main :: IO ()
main = do
    let z = f (MkT (bumpMarker 42))
    z `seq` pure ()
    n <- readIORef markerRef
    if n == 0 then putStrLn "not forced" else putStrLn "forced"
