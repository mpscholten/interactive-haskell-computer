module Main (main) where

import Test.Hspec

import qualified CabalLoader
import qualified CoreLowerTest
import qualified Coverage
import qualified HsExtSyntax
import qualified JitSmoke
import qualified LexerIhp
import qualified NorthStarTest
import qualified ParserBugs
import qualified ReplTest
import qualified RunFile
import qualified Unsupported

main :: IO ()
main = hspec do
    ParserBugs.spec
    HsExtSyntax.spec
    JitSmoke.spec
    CoreLowerTest.spec
    RunFile.spec
    CabalLoader.spec
    Coverage.spec
    Unsupported.spec
    ReplTest.spec
    NorthStarTest.spec
    LexerIhp.spec
