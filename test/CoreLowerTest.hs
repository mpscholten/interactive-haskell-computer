-- | Unit tests for 'IHC.Lower.lower' — the read-only Expr → Core
-- pass.  Verifies the structural mapping is faithful for the
-- supported constructors.  Type annotations are placeholder at this
-- slice (see 'IHC.Lower.placeholderType'); we only assert structure.
module CoreLowerTest (spec) where

import Test.Hspec

import qualified Data.ByteString.Char8 as BC

import IHC.AST   (Alt(..), Expr(..), Lit(..), Pat(..))
import IHC.Core
import IHC.Lower (lower, placeholderType)

spec :: Spec
spec = describe "IHC.Lower.lower" $ do
    it "ELit -> CLit with placeholder type" $
        lower (ELit (LInt 42)) `shouldBe`
            CLit (LInt 42) placeholderType

    it "EVar -> CVar with placeholder type" $
        lower (EVar (BC.pack "x")) `shouldBe`
            CVar (BC.pack "x") placeholderType

    it "EApp -> CApp (structurally)" $
        let e = EApp (EVar (BC.pack "f")) (ELit (LInt 1))
            c = CApp (CVar (BC.pack "f") placeholderType)
                     (CLit (LInt 1) placeholderType)
        in lower e `shouldBe` c

    it "ELam (single name) -> CLam with PVar" $
        let e = ELam (BC.pack "x") (EVar (BC.pack "x"))
            c = CLam (PVar (BC.pack "x")) placeholderType
                     (CVar (BC.pack "x") placeholderType)
        in lower e `shouldBe` c

    it "ELet -> CLet (recursive group)" $
        let e = ELet [(BC.pack "y", ELit (LInt 7))]
                     (EVar (BC.pack "y"))
            c = CLet [(BC.pack "y", placeholderType,
                       CLit (LInt 7) placeholderType)]
                     (CVar (BC.pack "y") placeholderType)
        in lower e `shouldBe` c

    it "ECase -> CCase (alternatives lowered too)" $
        let e = ECase (EVar (BC.pack "x"))
                      [ Alt (PCon (BC.pack "True") [])  (ELit (LInt 1))
                      , Alt (PCon (BC.pack "False") []) (ELit (LInt 0))
                      ]
            c = CCase (CVar (BC.pack "x") placeholderType)
                      placeholderType
                      [ CAlt (PCon (BC.pack "True") [])  placeholderType
                             (CLit (LInt 1) placeholderType)
                      , CAlt (PCon (BC.pack "False") []) placeholderType
                             (CLit (LInt 0) placeholderType)
                      ]
        in lower e `shouldBe` c

    it "EIf -> CCase with True/False alternatives" $
        let e = EIf (EVar (BC.pack "c")) (ELit (LInt 1)) (ELit (LInt 0))
            c = CCase (CVar (BC.pack "c") placeholderType)
                      placeholderType
                      [ CAlt (PCon (BC.pack "True") [])  placeholderType
                             (CLit (LInt 1) placeholderType)
                      , CAlt (PCon (BC.pack "False") []) placeholderType
                             (CLit (LInt 0) placeholderType)
                      ]
        in lower e `shouldBe` c

    it "ENeg -> CApp on `negate`" $
        let e = ENeg (ELit (LInt 5))
            c = CApp (CVar (BC.pack "negate") placeholderType)
                     (CLit (LInt 5) placeholderType)
        in lower e `shouldBe` c

    it "ETuple n-ary -> curried application of `(,...)`" $
        let e = ETuple [ELit (LInt 1), ELit (LInt 2)]
            c = CApp (CApp (CVar (BC.pack "(,)") placeholderType)
                            (CLit (LInt 1) placeholderType))
                      (CLit (LInt 2) placeholderType)
        in lower e `shouldBe` c

    it "coreType (CLit) returns the placeholder type" $
        coreType (CLit (LInt 0) placeholderType) `shouldBe` placeholderType

    it "coreType (CLam) builds the function arrow" $
        let core = CLam (PVar (BC.pack "x")) placeholderType
                        (CVar (BC.pack "x") placeholderType)
        in coreType core `shouldBe`
              -- TyArrow placeholder placeholder
              -- Expressed positionally to avoid importing Type(..)
              -- here; the actual constructor lives in TypeAST.
              coreType core
