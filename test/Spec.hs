module Main (main) where

import Test.Hspec

import qualified CabalLoader
import qualified CoreLowerTest
import qualified Coverage
import qualified HsExtTypeFams
import qualified HsExtMisc
import qualified HsExtRecords
import qualified Hs2010LexNum
import qualified HsExtTypeApps
import qualified HsExtForall
import qualified Hs2010Fixity
import qualified Hs2010Deriving
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
import qualified LexerIhp
import qualified NorthStarTest
import qualified ParserBugs
import qualified Properties.DoDesugar
import qualified Properties.RoundTrip
import qualified Properties.SectionDesugar
import qualified Properties.StringDesugar
import qualified Properties.Totality
import qualified Properties.TupleSectionDesugar
import qualified ReplTest
import qualified RunFile
import qualified Unsupported
import qualified NetworkSocketAddrInfoRecordUpdateTest
import qualified TopLevelIOBindingTest
import qualified TopLevelWarpAliasTest
import qualified WarpHelloTest
import qualified WarpRunStartupTest

main :: IO ()
main = hspec do
    ParserBugs.spec
    Properties.Totality.spec
    Properties.RoundTrip.spec
    Properties.SectionDesugar.spec
    Properties.DoDesugar.spec
    Properties.StringDesugar.spec
    Properties.TupleSectionDesugar.spec
    HsExtMisc.spec
    Hs2010LexNum.spec
    Hs2010Fixity.spec
    Hs2010Deriving.spec
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
    CoreLowerTest.spec
    RunFile.spec
    CabalLoader.spec
    Coverage.spec
    HsExtTypeFams.spec
    HsExtTypeApps.spec
    Unsupported.spec
    ReplTest.spec
    NorthStarTest.spec
    LexerIhp.spec
    HsExtRecords.spec
    HsExtForall.spec
    TopLevelIOBindingTest.spec
    TopLevelWarpAliasTest.spec
    WarpRunStartupTest.spec
    NetworkSocketAddrInfoRecordUpdateTest.spec
    WarpHelloTest.spec
