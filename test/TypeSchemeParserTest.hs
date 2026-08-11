module TypeSchemeParserTest (spec) where

import qualified Data.ByteString.Char8 as BC
import Test.Hspec

import IHC.Scan (scanTypeSigs)
import IHC.Source (mkSource)
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
  where
    agreesWithTopLevelScanner bytes = it ("matches scanTypeSigs for " ++ BC.unpack bytes) $ do
        scanned <- scanTypeSigs (mkSource "<scheme-parity>" ("value :: " <> bytes <> "\n"))
        parseSchemeBytes bytes `shouldBe` lookup "value" scanned
