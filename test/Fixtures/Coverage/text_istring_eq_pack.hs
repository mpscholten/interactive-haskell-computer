-- OverloadedStrings Text must equal T.pack of the same chars
-- and participate in Eq / Set.member. Pre-fix the literal stayed
-- a [Char]/VStr, so T.pack == OS and Set.member pack osSet hung
-- in source Eq/Ord Text (pattern-match Text arr off len on a string).
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Set as Set
import qualified Data.Text as T

main :: IO ()
main = do
    let a = T.pack "br"
        b = "br" :: T.Text
    putStrLn (if a == b then "eq" else "ne")
    let s = Set.fromList ["area","base","br","col" :: T.Text]
    putStrLn (if Set.member (T.pack "br") s then "has-br" else "no-br")
