module Main (main) where

import Test.Hspec

import qualified CabalLoader
import qualified CoreLowerTest
import qualified Coverage
import qualified Hs2010ExprCtl
import qualified Hs2010Modules
import qualified Hs2010LexStr
import qualified Hs2010ExprData
import qualified Hs2010LexLayout
import qualified Hs2010DataDecl
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
    Hs2010ExprCtl.spec
    Hs2010Modules.spec
    Hs2010LexStr.spec
    Hs2010ExprData.spec
    Hs2010LexLayout.spec
    Hs2010DataDecl.spec
    JitSmoke.spec
    CoreLowerTest.spec
    RunFile.spec
    CabalLoader.spec
    Coverage.spec
    Unsupported.spec
    ReplTest.spec
    NorthStarTest.spec
    LexerIhp.spec
