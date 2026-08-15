module TypeSchemeParserTest (spec) where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Test.Hspec

import IHC.ConstructorMetadata (constructorFieldTypes)
import IHC.Scan
    ( scanConstructorTypeMetadata
    , scanRecordSelectorSchemes
    , scanTypeSigs
    , scanTypeSynonyms
    )
import IHC.Source (mkSource)
import IHC.TypeAST (Pred(..), Scheme(..), Type(..), TypeSynonym(..), expandScheme, expandTypeSynonyms)
import IHC.TypeSchemeParser (parseSchemeBytes)

spec :: Spec
spec = describe "TypeSchemeParser" $ do
    mapM_ agreesWithTopLevelScanner
        [ "Int"
        , "a -> [a]"
        , "forall a. a -> a"
        , "(Monad m, Show a) => a -> m (Maybe a)"
        , "(forall a. C a => a -> a) -> (Int, Bool)"
        , "Pkg.Internal.Type.Name a"
        , "?callStack :: CallStack"
        , "(?x :: Int) => Int"
        , "(Eq a, ?x :: a) => a"
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
    it "parses an implicit-param-only scheme like HasCallStack's RHS" $
        parseSchemeBytes "(?callStack :: CallStack)" `shouldBe` Just
            (Scheme [] []
                (TyApp (TyCon "?callStack") (TyCon "CallStack")))
    it "registers type HasCallStack = (?callStack :: CallStack)" $ do
        syns <- scanTypeSynonyms (mkSource "GHC.Stack.Types.hs"
            "type HasCallStack = (?callStack :: CallStack)\n")
        lookup "HasCallStack" syns `shouldBe` Just
            (TypeSynonym []
                (TyApp (TyCon "?callStack") (TyCon "CallStack")))
    it "parses an unparenthesized implicit-param type ?callStack :: CallStack" $
        parseSchemeBytes "?callStack :: CallStack" `shouldBe` Just
            (Scheme [] []
                (TyApp (TyCon "?callStack") (TyCon "CallStack")))
    it "parses a single implicit-param constraint (?x :: Int) => Int" $
        parseSchemeBytes "(?x :: Int) => Int" `shouldBe` Just
            (Scheme [] [Pred "?x" [TyCon "Int"]] (TyCon "Int"))
    it "parses mixed class and implicit-param context (Eq a, ?x :: a) => a" $
        parseSchemeBytes "(Eq a, ?x :: a) => a" `shouldBe` Just
            (Scheme ["a"]
                [Pred "Eq" [TyVar "a"], Pred "?x" [TyVar "a"]]
                (TyVar "a"))
    it "registers nested synonyms of implicit-param constraints" $ do
        syns <- scanTypeSynonyms (mkSource "NestedIP.hs"
            "type HasCallStack = (?callStack :: CallStack)\n\
            \type AlsoHasCallStack = HasCallStack\n\
            \type NeedsStack a = HasCallStack => a\n\
            \type InnerIP = (?x :: Int)\n\
            \type NestedIP = (?y :: Bool) => InnerIP\n")
        lookup "HasCallStack" syns `shouldBe` Just
            (TypeSynonym []
                (TyApp (TyCon "?callStack") (TyCon "CallStack")))
        lookup "AlsoHasCallStack" syns `shouldBe` Just
            (TypeSynonym [] (TyCon "HasCallStack"))
        lookup "NeedsStack" syns `shouldBe` Just
            (TypeSynonym ["a"] (TyVar "a"))
        lookup "InnerIP" syns `shouldBe` Just
            (TypeSynonym [] (TyApp (TyCon "?x") (TyCon "Int")))
        lookup "NestedIP" syns `shouldBe` Just
            (TypeSynonym [] (TyCon "InnerIP"))
        let synMap = Map.fromList syns
        expandTypeSynonyms synMap (TyCon "AlsoHasCallStack") `shouldBe`
            TyApp (TyCon "?callStack") (TyCon "CallStack")
        expandTypeSynonyms synMap (TyCon "NestedIP") `shouldBe`
            TyApp (TyCon "?x") (TyCon "Int")
    it "scans implicit-param schemes as type signatures" $ do
        scanned <- scanTypeSigs (mkSource "S.hs"
            "bare :: ?callStack :: CallStack\n\
            \plain :: (?x :: Int) => Int\n\
            \mixed :: (Eq a, ?x :: a) => a\n")
        lookup "bare" scanned `shouldBe` Just
            (Scheme [] [] (TyApp (TyCon "?callStack") (TyCon "CallStack")))
        lookup "plain" scanned `shouldBe` Just
            (Scheme [] [Pred "?x" [TyCon "Int"]] (TyCon "Int"))
        lookup "mixed" scanned `shouldBe` Just
            (Scheme ["a"]
                [Pred "Eq" [TyVar "a"], Pred "?x" [TyVar "a"]]
                (TyVar "a"))
    it "keeps a nullary constraint synonym like HasCallStack" $
        parseSchemeBytes "HasCallStack => String" `shouldBe` Just
            (Scheme [] [Pred "HasCallStack" []] (TyCon "String"))
    it "expands HasCallStack on a scheme into the ?callStack IP pred" $ do
        syns <- Map.fromList <$> scanTypeSynonyms (mkSource "S.hs"
            "type HasCallStack = (?callStack :: CallStack)\n")
        let sch = Scheme [] [Pred "HasCallStack" []] (TyCon "String")
        expandScheme syns sch `shouldBe`
            Scheme [] [Pred "?callStack" [TyCon "CallStack"]] (TyCon "String")
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
    -- Warp / blaze leftovers: keep H98 alts when a later alternative is
    -- an unsupported `forall` (MarkupM Append), and pin Warp Settings /
    -- http-types Status selector schemes.
    it "leftover: keeps H98 alts when a later alt is unsupported forall (MarkupM)" $ do
        reg <- scanConstructorTypeMetadata "Blaze" (mkSource "Markup.hs"
            "data MarkupM a\n\
            \    = Parent String String String (MarkupM a)\n\
            \    | Content ChoiceString\n\
            \    | forall b. Append (MarkupM b) (MarkupM a)\n\
            \    | Empty\n")
        let markupA = TyApp (TyCon "MarkupM") (TyVar "a")
        constructorFieldTypes reg (Just "Blaze") "Content" markupA
            `shouldBe` Just [TyCon "ChoiceString"]
        constructorFieldTypes reg (Just "Blaze") "Empty" markupA
            `shouldBe` Just []
        constructorFieldTypes reg (Just "Blaze") "Parent" markupA
            `shouldBe` Just
                [ TyCon "String"
                , TyCon "String"
                , TyCon "String"
                , markupA
                ]
        constructorFieldTypes reg (Just "Blaze") "Append" markupA
            `shouldBe` Nothing
    it "leftover: warp Settings settingsPort / settingsHost selector schemes" $ do
        fields <- scanRecordSelectorSchemes (mkSource "Settings.hs"
            "data Settings = Settings { settingsPort :: Port, settingsHost :: HostPreference }\n")
        Map.lookup "settingsPort" fields `shouldBe` Just
            [("Settings", Scheme [] []
                (TyArrow (TyCon "Settings") (TyCon "Port")))]
        Map.lookup "settingsHost" fields `shouldBe` Just
            [("Settings", Scheme [] []
                (TyArrow (TyCon "Settings") (TyCon "HostPreference")))]
    it "leftover: http-types Status statusCode / statusMessage selector schemes" $ do
        fields <- scanRecordSelectorSchemes (mkSource "Status.hs"
            "data Status = Status { statusCode :: Int, statusMessage :: ByteString }\n")
        Map.lookup "statusCode" fields `shouldBe` Just
            [("Status", Scheme [] []
                (TyArrow (TyCon "Status") (TyCon "Int")))]
        Map.lookup "statusMessage" fields `shouldBe` Just
            [("Status", Scheme [] []
                (TyArrow (TyCon "Status") (TyCon "ByteString")))]
    it "leftover: warp rank-n settingsFork field scheme is retained" $ do
        fields <- scanRecordSelectorSchemes (mkSource "Fork.hs"
            "data Settings = Settings { settingsFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO () }\n")
        Map.lookup "settingsFork" fields `shouldBe` Just
            [("Settings", Scheme [] []
                (TyArrow (TyCon "Settings")
                    (TyArrow
                        (TyArrow
                            (TyForall ["a"] []
                                (TyArrow (TyApp (TyCon "IO") (TyVar "a"))
                                         (TyApp (TyCon "IO") (TyVar "a"))))
                            (TyApp (TyCon "IO") (TyCon "()")))
                        (TyApp (TyCon "IO") (TyCon "()")))))]
  where
    agreesWithTopLevelScanner bytes = it ("matches scanTypeSigs for " ++ BC.unpack bytes) $ do
        scanned <- scanTypeSigs (mkSource "<scheme-parity>" ("value :: " <> bytes <> "\n"))
        parseSchemeBytes bytes `shouldBe` lookup "value" scanned
