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

-- | 'parseModuleHeader' never throws (it gives up gracefully on malformed
-- input) so the result is always a clean tuple.
parseHeader :: ByteString -> IO (Maybe ModuleHeader)
parseHeader bs = fst <$> parseModuleHeader (mkSource "<test>" bs) startCursor

-- | Run 'parseHeader' and dispatch on the result; on a structural mismatch,
-- 'expectationFailure' reports the actual header so the test output points
-- straight at the parser's interpretation.
withHeader :: ByteString -> (Maybe ModuleHeader -> Maybe Expectation) -> Expectation
withHeader bs k = do
    mh <- parseHeader bs
    case k mh of
        Just expectation -> expectation
        Nothing -> expectationFailure ("unexpected header: " <> show mh)

spec :: Spec
spec = describe "Hs2010 — Modules" $ do

    describe "2.1 module headers" $ do
        it "2.1.1 module M(x) where" $
            withHeader "module M(x) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportName "x"])
                _ -> Nothing

        it "2.1.2 module M where" $
            withHeader "module M where\n" $ \case
                Just (ModuleHeader (Just "M") ExportAll _) -> Just (pure ())
                _ -> Nothing

        it "2.1.3 hierarchical module A.B where" $
            withHeader "module A.B where\n" $ \case
                Just (ModuleHeader (Just "A.B") ExportAll _) -> Just (pure ())
                _ -> Nothing

        it "2.1.4 abbreviated (no header) returns Nothing" $
            withHeader "x = 1\n" $ \case
                Nothing -> Just (pure ())
                _ -> Nothing

    describe "2.2 export-list items" $ do
        it "2.2.1 module M() where (empty list)" $
            withHeader "module M() where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList []) _) -> Just (pure ())
                _ -> Nothing

        it "2.2.2 trailing comma (x,)" $
            withHeader "module M(x,) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportName "x"])
                _ -> Nothing

        it "2.2.3 export a value (foo)" $
            withHeader "module M(foo) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportName "foo"])
                _ -> Nothing

        it "2.2.4 qualified value (M.foo)" $
            withHeader "module M(B.foo) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportName "foo"])
                _ -> Nothing

        it "2.2.5 parenthesised operator ((+))" $
            withHeader "module M((+)) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportName "+"])
                _ -> Nothing

        it "2.2.6 type only (T)" $
            withHeader "module M(T) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportType "T" Nothing])
                _ -> Nothing

        it "2.2.7 type with all constructors (T(..))" $
            withHeader "module M(T(..)) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportType "T" (Just [])])
                _ -> Nothing

        it "2.2.8 type with selected constructors (T(C1,C2))" $
            withHeader "module M(T(C1,C2)) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportType "T" (Just ["C1","C2"])])
                _ -> Nothing

        it "2.2.9 class only (C)" $
            withHeader "module M(C) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportType "C" Nothing])
                _ -> Nothing

        it "2.2.10 class with all methods (C(..))" $
            withHeader "module M(C(..)) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportType "C" (Just [])])
                _ -> Nothing

        it "2.2.11 class with selected methods (C(m1,m2))" $
            withHeader "module M(C(m1,m2)) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportType "C" (Just ["m1","m2"])])
                _ -> Nothing

        it "2.2.12 re-export (module M)" $
            withHeader "module M(module N) where\n" $ \case
                Just (ModuleHeader (Just "M") (ExportList items) _) ->
                    Just (items `shouldBe` [ExportModule "N"])
                _ -> Nothing

    describe "2.3 import declarations" $ do
        it "2.3.1 plain import M" $
            withHeader "module M where\nimport N\n" $ \case
                Just (ModuleHeader _ _ [ImportDecl "N" False Nothing ImportAll]) ->
                    Just (pure ())
                _ -> Nothing

        it "2.3.2 import qualified M" $
            withHeader "module M where\nimport qualified N\n" $ \case
                Just (ModuleHeader _ _ [ImportDecl "N" True Nothing ImportAll]) ->
                    Just (pure ())
                _ -> Nothing

        it "2.3.3 import M as N" $
            withHeader "module M where\nimport N as O\n" $ \case
                Just (ModuleHeader _ _
                    [ImportDecl "N" False (Just "O") ImportAll]) -> Just (pure ())
                _ -> Nothing

        it "2.3.4 import qualified M as N" $
            withHeader "module M where\nimport qualified N as O\n" $ \case
                Just (ModuleHeader _ _
                    [ImportDecl "N" True (Just "O") ImportAll]) -> Just (pure ())
                _ -> Nothing

        it "2.3.5 selective import M (x,y)" $
            withHeader "module M where\nimport N (x,y)\n" $ \case
                Just (ModuleHeader _ _
                    [ImportDecl "N" False Nothing (ImportOnly ["x","y"])]) ->
                    Just (pure ())
                _ -> Nothing

        it "2.3.6 import M (T(..))" $
            withHeader "module M where\nimport N (T(..))\n" $ \case
                Just (ModuleHeader _ _ [ImportDecl "N" False Nothing
                    (ImportOnly names)]) ->
                    Just ("T" `elem` names `shouldBe` True)
                _ -> Nothing

        it "2.3.7 import M hiding (x)" $
            withHeader "module M where\nimport N hiding (x)\n" $ \case
                Just (ModuleHeader _ _
                    [ImportDecl "N" False Nothing (ImportHiding ["x"])]) ->
                    Just (pure ())
                _ -> Nothing

        it "2.3.8 import M hiding ()" $
            withHeader "module M where\nimport N hiding ()\n" $ \case
                Just (ModuleHeader _ _
                    [ImportDecl "N" False Nothing (ImportHiding [])]) ->
                    Just (pure ())
                _ -> Nothing

        it "2.3.9 import M () (instances only)" $
            withHeader "module M where\nimport N ()\n" $ \case
                Just (ModuleHeader _ _
                    [ImportDecl "N" False Nothing (ImportOnly [])]) ->
                    Just (pure ())
                _ -> Nothing

        it "2.3.10 trailing comma in import list (x,)" $
            withHeader "module M where\nimport N (x,)\n" $ \case
                Just (ModuleHeader _ _
                    [ImportDecl "N" False Nothing (ImportOnly ["x"])]) ->
                    Just (pure ())
                _ -> Nothing

        it "2.3.11 ctor in hiding (C) (no type prefix)" $
            withHeader "module M where\nimport N hiding (C)\n" $ \case
                Just (ModuleHeader _ _
                    [ImportDecl "N" False Nothing (ImportHiding ["C"])]) ->
                    Just (pure ())
                _ -> Nothing
