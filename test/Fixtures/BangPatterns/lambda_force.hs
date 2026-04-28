-- A.1 — Haskell Report §3.17.2: `\(!x) -> body` must force x to WHNF
-- on apply, even when body discards x.  Detection via an IORef bumped
-- from inside a thunk passed as the lambda's argument.
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

f :: Int -> Int
f = \(!_x) -> 0

main :: IO ()
main = do
    let z = f (bumpMarker (42 :: Int))
    -- Reference z so the application happens.  Its result is 0
    -- regardless of whether the bang fires; we use the marker as
    -- the actual signal.
    z `seq` pure ()
    n <- readIORef markerRef
    if n == 0 then putStrLn "not forced" else putStrLn "forced"
