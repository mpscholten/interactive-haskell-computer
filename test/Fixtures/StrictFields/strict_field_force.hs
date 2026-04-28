-- A.5 — Haskell Report §4.2.1: a strict field annotation
-- (`MkT !Int Int`) forces the corresponding argument to WHNF at
-- construction time.  Detection via an IORef bumped from inside the
-- thunk passed to the strict field — if the bang fires, the marker
-- is non-zero by the time we read it.
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

data T = MkT !Int Int

main :: IO ()
main = do
    let _ = MkT (bumpMarker 1) (bumpMarker 99)
        -- ^ second field is lazy; bumpMarker never fires there.
        --   first field is strict; ihc must force it on construction.
    -- Force the construction by case-matching to WHNF.
    case MkT (bumpMarker 7) (bumpMarker 99) of
        _ -> pure ()
    n <- readIORef markerRef
    if n == 1
        then putStrLn "forced"
        else putStrLn ("not forced (n = " <> show n <> ")")
