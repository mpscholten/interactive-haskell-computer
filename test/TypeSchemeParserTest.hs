module TypeSchemeParserTest (spec) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Test.Hspec

import IHC.Scan (scanRecordSelectorSchemes, scanTypeSigs, scanTypeSynonyms)
import IHC.Source (mkSource)
import IHC.TypeAST (Pred(..), Scheme(..), Type(..), TypeSynonym(..))
import IHC.TypeSchemeParser (parseSchemeBytes)

spec :: Spec
spec = describe "shared type-scheme parser" $ do
    mapM_ agreesWithTopLevelScanner
        [ "Int"
        , "a -> [a]"
        , "forall a. a -> a"
        , "(Monad m, Show a) => a -> m (Maybe a)"
        , "(forall a. C a => a -> a) -> (Int, Bool)"
        , "Pkg.Internal.Type.Name a"
        ]
    it "rejects tokens outside the type grammar" $
        parseSchemeBytes "Int = Bool" `shouldBe` Nothing
    it "strips implicit-param context from a scheme" $
        parseSchemeBytes "(?ok :: Bool) => Parsec Void Text a" `shouldBe` Just
            (Scheme ["a"]
                [Pred "?ok" [TyCon "Bool"]]
                (TyApp (TyApp (TyApp (TyCon "Parsec") (TyCon "Void"))
                    (TyCon "Text")) (TyVar "a")))
    it "strips two implicit-param constraints like HSX Parser" $
        parseSchemeBytes
            "(?extensions :: [()], ?settings :: Settings) => Parsec Void Text a"
            `shouldBe` Just
            (Scheme ["a"]
                [ Pred "?extensions" [TyApp (TyCon "[]") (TyCon "()")]
                , Pred "?settings" [TyCon "Settings"]
                ]
                (TyApp (TyApp (TyApp (TyCon "Parsec") (TyCon "Void"))
                    (TyCon "Text")) (TyVar "a")))
    it "registers a constrained type-synonym body as the carrier type" $ do
        syns <- scanTypeSynonyms (mkSource "P.hs"
            "type Parser a = (?ok :: Bool) => Parsec Void Text a\n")
        lookup "Parser" syns `shouldBe` Just
            (TypeSynonym ["a"]
                (TyApp (TyApp (TyApp (TyCon "Parsec") (TyCon "Void"))
                    (TyCon "Text")) (TyVar "a")))
    it "retains a QuasiQuoter-like record selector scheme" $ do
        fields <- scanRecordSelectorSchemes (mkSource "Q.hs"
            "data QuasiQuoter = QuasiQuoter { quoteExp :: String -> Q Exp, other :: Int }\n")
        Map.lookup "quoteExp" fields `shouldBe` Just
            [("QuasiQuoter", Scheme [] []
                (TyArrow (TyCon "QuasiQuoter")
                    (TyArrow (TyCon "String") (TyApp (TyCon "Q") (TyCon "Exp")))))]
    it "retains grouped H98 record labels" $ do
        fields <- scanRecordSelectorSchemes (mkSource "Grouped.hs"
            "data Pair = Pair { x, y :: Int }\n")
        let expected = [("Pair", Scheme [] [] (TyArrow (TyCon "Pair") (TyCon "Int")))]
        Map.lookup "x" fields `shouldBe` Just expected
        Map.lookup "y" fields `shouldBe` Just expected
    it "retains datatype contexts in selector evidence" $ do
        fields <- scanRecordSelectorSchemes (mkSource "Context.hs"
            "data Eq a => Wrapped a = Wrapped { unwrap :: a }\n")
        Map.lookup "unwrap" fields `shouldBe` Just
            [("Wrapped", Scheme ["a"] [Pred "Eq" [TyVar "a"]]
                (TyArrow (TyApp (TyCon "Wrapped") (TyVar "a")) (TyVar "a")))]
    it "retains compatible duplicate selector alternatives" $ do
        fields <- scanRecordSelectorSchemes (mkSource "Dup.hs"
            "data A = A { value :: Int }\ndata B = B { value :: Int }\n")
        Map.findWithDefault [] "value" fields `shouldMatchList`
            [ ("A", Scheme [] [] (TyArrow (TyCon "A") (TyCon "Int")))
            , ("B", Scheme [] [] (TyArrow (TyCon "B") (TyCon "Int")))
            ]
    it "retains incompatible duplicates separately for fail-closed consumers" $ do
        fields <- scanRecordSelectorSchemes (mkSource "DupBad.hs"
            "data A = A { value :: Int }\ndata B = B { value :: Bool }\n")
        Map.findWithDefault [] "value" fields `shouldMatchList`
            [ ("A", Scheme [] [] (TyArrow (TyCon "A") (TyCon "Int")))
            , ("B", Scheme [] [] (TyArrow (TyCon "B") (TyCon "Bool")))
            ]
  where
    agreesWithTopLevelScanner bytes = it ("matches scanTypeSigs for " ++ BC.unpack bytes) $ do
        scanned <- scanTypeSigs (mkSource "<scheme-parity>" ("value :: " <> bytes <> "\n"))
        parseSchemeBytes bytes `shouldBe` lookup "value" scanned
