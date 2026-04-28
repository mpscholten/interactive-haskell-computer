-- A.1 — Haskell Report §3.17.2: `let !x = e` inside a do-block must
-- force e to WHNF before the rest of the block runs, even when x is
-- never referenced.  We detect this via an IORef bumped from inside
-- a thunk: if the bang fires, the marker is non-zero by the time we
-- read it.
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
    let !_x = bumpMarker (42 :: Int)
    n <- readIORef markerRef
    if n == 0 then putStrLn "not forced" else putStrLn "forced"
