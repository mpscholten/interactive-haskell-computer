-- Natural NS / NB constructor bridge (mirror of Integer IS / IP / IN).
--
-- ghc-bignum: data Natural = NS !Word# | NB !BigNat#
-- Literals and fromInteger land as VInt; source that patterns on NS
-- must still match.  Without the bridge, integerFromNatural and
-- warp's post-recv Natural/Integer conversions PatternMatchFail.
module Main where

import GHC.Num.Integer (integerFromNatural, integerToInt#)
import GHC.Num.Natural (Natural(..))
import GHC.Exts (Int(..), Word(..))

main :: IO ()
main = do
    -- Literal Natural as NS via match bridge
    case (5 :: Natural) of
        NS w -> print (W# w)
        NB _ -> putStrLn "NB"
    -- integerFromNatural end-to-end
    print (I# (integerToInt# (integerFromNatural (7 :: Natural))))
    print (I# (integerToInt# (integerFromNatural (0 :: Natural))))
