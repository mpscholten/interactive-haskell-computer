module Main (main) where

import Test.Hspec

import qualified CabalLoader
import qualified CoreLowerTest
import qualified Coverage
import qualified HsExtDeriving
import qualified HsExtSyntax
import qualified HsExtClasses
import qualified HsExtKinds
import qualified HsExtPatterns
import qualified Hs2010ClassInst
import qualified Hs2010Types
import qualified HsExtGADTs
import qualified Hs2010Patterns
import qualified Hs2010LexIdent
import qualified Hs2010Bindings
import qualified Hs2010LexComments
import qualified HsExtLiterals
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
    HsExtDeriving.spec
    HsExtSyntax.spec
    HsExtClasses.spec
    HsExtKinds.spec
    HsExtPatterns.spec
    Hs2010ClassInst.spec
    Hs2010Types.spec
    HsExtGADTs.spec
    Hs2010Patterns.spec
    Hs2010LexIdent.spec
    Hs2010Bindings.spec
    Hs2010LexComments.spec
    HsExtLiterals.spec
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
