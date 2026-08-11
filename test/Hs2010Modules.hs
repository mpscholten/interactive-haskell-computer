{-# LANGUAGE OverloadedStrings #-}

-- | Haskell 2010 §2 (Modules) parser-conformance audit.  Every numbered
-- item in §2.1 / §2.2 / §2.3 of the parser-coverage taxonomy gets a
-- dedicated 'it', exercising 'parseModuleHeader' against the same
-- syntactic shape GHC accepts.
module Hs2010Modules (spec) where

import Data.ByteString (ByteString)
import Test.Hspec

import IHC.Lexer (startCursor)
import IHC.ModuleHeader
    ( ExportItem(..)
    , ExportSpec(..)
    , ImportDecl(..)
    , ImportSpec(..)
    , ModuleHeader(..)
    , parseModuleHeader
    )
import IHC.Source (mkSource)

parseHeader :: ByteString -> IO (Maybe ModuleHeader)
parseHeader bs = fst <$> parseModuleHeader (mkSource "<test>" bs) startCursor

shouldParseHeaderTo :: ByteString -> ModuleHeader -> Expectation
shouldParseHeaderTo bs expected = do
    actual <- parseHeader bs
    actual `shouldBe` Just expected

shouldParseToNoHeader :: ByteString -> Expectation
shouldParseToNoHeader bs = do
    actual <- parseHeader bs
    actual `shouldBe` Nothing

mh :: ByteString -> ExportSpec -> [ImportDecl] -> ModuleHeader
mh name = ModuleHeader (Just name)

spec :: Spec
spec = describe "Hs2010 — Modules" $ do

    describe "2.1 module headers" $ do
        it "2.1.1 module M(x) where" $
            "module M(x) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportName "x"]) []

        it "2.1.2 module M where" $
            "module M where\n" `shouldParseHeaderTo`
                mh "M" ExportAll []

        it "2.1.3 hierarchical module A.B where" $
            "module A.B where\n" `shouldParseHeaderTo`
                mh "A.B" ExportAll []

        it "2.1.4 abbreviated (no header) returns Nothing" $
            shouldParseToNoHeader "x = 1\n"

    describe "2.2 export-list items" $ do
        it "2.2.1 module M() where (empty list)" $
            "module M() where\n" `shouldParseHeaderTo`
                mh "M" (ExportList []) []

        it "2.2.2 trailing comma (x,)" $
            "module M(x,) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportName "x"]) []

        it "2.2.3 export a value (foo)" $
            "module M(foo) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportName "foo"]) []

        it "2.2.4 qualified value (M.foo)" $
            "module M(B.foo) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportName "foo"]) []

        it "2.2.5 parenthesised operator ((+))" $
            "module M((+)) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportName "+"]) []

        it "keeps multi-token operators and following exports" $
            "module M((.&.), (@?=), value) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList
                    [ExportName ".&.", ExportName "@?=", ExportName "value"]) []

        it "keeps multi-token class-child operators" $
            "module M(C((.&.), (@?=), method), value) where\n"
                `shouldParseHeaderTo`
                    mh "M" (ExportList
                        [ ExportType "C" (Just [".&.", "@?=", "method"])
                        , ExportName "value"
                        ]) []

        it "2.2.6 type only (T)" $
            "module M(T) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportType "T" Nothing]) []

        it "2.2.7 type with all constructors (T(..))" $
            "module M(T(..)) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportType "T" (Just [])]) []

        it "2.2.8 type with selected constructors (T(C1,C2))" $
            "module M(T(C1,C2)) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportType "T" (Just ["C1","C2"])]) []

        it "2.2.9 class only (C)" $
            "module M(C) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportType "C" Nothing]) []

        it "2.2.10 class with all methods (C(..))" $
            "module M(C(..)) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportType "C" (Just [])]) []

        it "2.2.11 class with selected methods (C(m1,m2))" $
            "module M(C(m1,m2)) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportType "C" (Just ["m1","m2"])]) []

        it "2.2.12 re-export (module M)" $
            "module M(module N) where\n" `shouldParseHeaderTo`
                mh "M" (ExportList [ExportModule "N"]) []

    describe "2.3 import declarations" $ do
        it "2.3.1 plain import M" $
            "module M where\nimport N\n" `shouldParseHeaderTo`
                mh "M" ExportAll [ImportDecl "N" False Nothing ImportAll]

        it "2.3.2 import qualified M" $
            "module M where\nimport qualified N\n" `shouldParseHeaderTo`
                mh "M" ExportAll [ImportDecl "N" True Nothing ImportAll]

        it "2.3.3 import M as N" $
            "module M where\nimport N as O\n" `shouldParseHeaderTo`
                mh "M" ExportAll [ImportDecl "N" False (Just "O") ImportAll]

        it "2.3.4 import qualified M as N" $
            "module M where\nimport qualified N as O\n" `shouldParseHeaderTo`
                mh "M" ExportAll [ImportDecl "N" True (Just "O") ImportAll]

        it "2.3.5 selective import M (x,y)" $
            "module M where\nimport N (x,y)\n" `shouldParseHeaderTo`
                mh "M" ExportAll
                    [ImportDecl "N" False Nothing (ImportOnly ["x","y"])]

        it "keeps multi-token operators and following selective imports" $
            "module M where\nimport N ((.&.), (@?=), value)\n"
                `shouldParseHeaderTo`
                    mh "M" ExportAll
                        [ImportDecl "N" False Nothing
                            (ImportOnly [".&.", "@?=", "value"])]

        it "keeps multi-token operators and following hidden imports" $
            "module M where\nimport N hiding ((.&.), (@?=), value)\n"
                `shouldParseHeaderTo`
                    mh "M" ExportAll
                        [ImportDecl "N" False Nothing
                            (ImportHiding [".&.", "@?=", "value"])]

        it "2.3.6 import M (T(..))" $
            "module M where\nimport N (T(..))\n" `shouldParseHeaderTo`
                mh "M" ExportAll
                    [ImportDecl "N" False Nothing (ImportOnly ["T","$dotdot:T"])]

        it "2.3.7 import M hiding (x)" $
            "module M where\nimport N hiding (x)\n" `shouldParseHeaderTo`
                mh "M" ExportAll
                    [ImportDecl "N" False Nothing (ImportHiding ["x"])]

        it "2.3.8 import M hiding ()" $
            "module M where\nimport N hiding ()\n" `shouldParseHeaderTo`
                mh "M" ExportAll
                    [ImportDecl "N" False Nothing (ImportHiding [])]

        it "2.3.9 import M () (instances only)" $
            "module M where\nimport N ()\n" `shouldParseHeaderTo`
                mh "M" ExportAll
                    [ImportDecl "N" False Nothing (ImportOnly [])]

        it "2.3.10 trailing comma in import list (x,)" $
            "module M where\nimport N (x,)\n" `shouldParseHeaderTo`
                mh "M" ExportAll
                    [ImportDecl "N" False Nothing (ImportOnly ["x"])]

        it "2.3.11 ctor in hiding (C) (no type prefix)" $
            "module M where\nimport N hiding (C)\n" `shouldParseHeaderTo`
                mh "M" ExportAll
                    [ImportDecl "N" False Nothing (ImportHiding ["C"])]

        it "PackageImports import \"pkg\" M ignores package qualifier" $
            "module M where\nimport \"base\" Data.List (sort)\n" `shouldParseHeaderTo`
                mh "M" ExportAll
                    [ImportDecl "Data.List" False Nothing (ImportOnly ["sort"])]

        it "PackageImports keeps qualified alias and import list" $
            "module M where\nimport qualified \"text\" Data.Text as T (pack)\n"
                `shouldParseHeaderTo`
                    mh "M" ExportAll
                        [ImportDecl "Data.Text" True (Just "T") (ImportOnly ["pack"])]
