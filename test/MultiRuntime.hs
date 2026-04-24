module MultiRuntime (spec) where

import qualified Data.ByteString.Char8 as BC
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map

import Test.Hspec

import IHC.Eval (force)
import IHC.Runtime (IHCRuntime(..), newIHCRuntime)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (mkSource)
import IHC.Val (Val(..))

-- | Regression test: the refactor that moved per-run state from
-- 'unsafePerformIO' CAFs onto 'IHCRuntime' means two runtimes created
-- in the same process should not cross-contaminate.  Before the
-- refactor, the second 'loadProgramFromSource' call would inherit the
-- first run's 'globalLoadedModulesRef' / 'envFallbackCache' /
-- 'globalTypeSigsRef' / … , which made "fresh interpreter" a fiction.
spec :: Spec
spec = describe "IHCRuntime multi-instance isolation" do
    it "two runtimes run independent programs without cross-talk" do
        let src1 = mkSource "<rt1>" (BC.pack "main = 1\n")
            src2 = mkSource "<rt2>" (BC.pack "main = 2\n")
        rt1 <- newIHCRuntime
        rt2 <- newIHCRuntime
        (_env1, main1) <- loadProgramFromSource rt1 [] src1
        (_env2, main2) <- loadProgramFromSource rt2 [] src2
        v1 <- force main1
        v2 <- force main2
        vAsInt v1 `shouldBe` Just 1
        vAsInt v2 `shouldBe` Just 2

    it "a fresh runtime starts with an empty loaded-modules map" do
        rt <- newIHCRuntime
        loaded <- readIORef (rtLoadedModules rt)
        Map.size loaded `shouldBe` 0

    it "a fresh runtime starts with an empty env-fallback cache" do
        rt <- newIHCRuntime
        cache <- readIORef (rtEnvFallbackCache rt)
        Map.size cache `shouldBe` 0

vAsInt :: Val -> Maybe Int
vAsInt (VInt n) = Just (fromIntegral n)
vAsInt _        = Nothing
