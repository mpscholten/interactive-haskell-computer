module BuiltinsAudit (spec) where

import Test.Hspec

import IHC.Builtins (builtinEnv)
import IHC.Classes (newClassRegistry)
import IHC.Val (lookupEnv)

spec :: Spec
spec = describe "Typeable builtin boundary" do
    it "does not register removed pre-Type.Reflection constructors" do
        registry <- newClassRegistry
        env <- builtinEnv registry
        isNothing (lookupEnv "mkTyCon3" env) `shouldBe` True
        isNothing (lookupEnv "mkTyConApp" env) `shouldBe` True

    it "retains the compiler-generated Typeable dictionary bridge" do
        registry <- newClassRegistry
        env <- builtinEnv registry
        isJust (lookupEnv "typeRep" env) `shouldBe` True
  where
    isNothing Nothing = True
    isNothing _       = False
    isJust (Just _) = True
    isJust _        = False
