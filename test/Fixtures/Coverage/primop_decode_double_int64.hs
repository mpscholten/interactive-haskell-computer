-- Smoke test for 'decodeDouble_Int64#' and the Int64# /
-- Int#-bitwise primop registrations.  These primops are
-- infrastructure for the (still-deferred) source-loaded
-- floor/ceiling/round/truncate graduation: the chain runs
--
--   floor x  →  floorFloat x
--            →  properFractionFloat
--            →  decodeFloat (D# x#)
--            →  integerDecodeDouble# x#
--            →  decodeDouble_Int64# x#
--
-- This fixture pins the bottom-of-stack primop directly so a
-- regression in 'decodeDouble_Int64#' is caught even before the
-- upstream graduation lands.  The expected result for 1.5 is
-- mantissa = 0x18000000000000 (= 6755399441055744 = 1.5 * 2^52)
-- and exponent = -52, so 1.5 = 6755399441055744 * 2^-52.
module Main where

import GHC.Prim (decodeDouble_Int64#)
import GHC.Types (Double(..))
import GHC.Int (Int64(..))
import GHC.Exts (Int(..))

main :: IO ()
main = do
    case 1.5 :: Double of
        D# d -> case decodeDouble_Int64# d of
            (# m, e #) -> do
                putStrLn (show (I64# m) ++ " * 2^" ++ show (I# e) ++ " = 1.5")
    case 0.0 :: Double of
        D# d -> case decodeDouble_Int64# d of
            (# m, e #) -> do
                putStrLn ("decodeDouble_Int64# 0.0 = (" ++ show (I64# m)
                          ++ ", " ++ show (I# e) ++ ")")
