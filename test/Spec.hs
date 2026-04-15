module Main (main) where

import Test.Hspec

import qualified CabalLoader
import qualified JitSmoke
import qualified RunFile

main :: IO ()
main = hspec do
    JitSmoke.spec
    RunFile.spec
    CabalLoader.spec
