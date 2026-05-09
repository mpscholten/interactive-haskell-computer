{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Modules.PatsynXmodReexport.Lib (Box(BS, Pair)) where

data Box = BS Int Int

pattern Pair :: Int -> Int -> Int -> Box
pattern Pair fp zero len <- BS ((0,) -> (zero, len)) fp where
    Pair fp o len = BS (fp + o) len
