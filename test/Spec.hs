module Main (main) where

import Test.Hspec

import qualified JitSmoke
import qualified RunFile

main :: IO ()
main = hspec do
    JitSmoke.spec
    RunFile.spec
