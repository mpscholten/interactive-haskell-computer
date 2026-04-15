module Main (main) where

import Test.Hspec

import qualified JitSmoke

main :: IO ()
main = hspec do
    JitSmoke.spec
