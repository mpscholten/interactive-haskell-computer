module Main (main) where

import Test.Hspec

import qualified CabalLoader
import qualified Coverage
import qualified JitSmoke
import qualified ReplTest
import qualified RunFile

main :: IO ()
main = hspec do
    JitSmoke.spec
    RunFile.spec
    CabalLoader.spec
    Coverage.spec
    ReplTest.spec
