module Main (main) where

import Test.Hspec

import qualified CabalLoader
import qualified CoreLowerTest
import qualified Coverage
import qualified JitSmoke
import qualified LexerIhp
import qualified NorthStarTest
import qualified ReplTest
import qualified RunFile
import qualified Unsupported

main :: IO ()
main = hspec do
    JitSmoke.spec
    CoreLowerTest.spec
    RunFile.spec
    CabalLoader.spec
    Coverage.spec
    Unsupported.spec
    ReplTest.spec
    NorthStarTest.spec
    LexerIhp.spec
