module RunFile (spec) where

import Test.Hspec

import IHC.Driver

spec :: Spec
spec = describe "Phase 1.0 — demand-driven single-pass JIT" do
    it "runs `main = 42`" do
        n <- runFile "test/Fixtures/lit42.hs"
        n `shouldBe` 42

    it "emits movz+movk chain for a large 64-bit literal" do
        n <- runFile "test/Fixtures/lit_large.hs"
        -- 0x0123_4567_89AB_CDEF = 81985529216486895
        n `shouldBe` 81985529216486895

    it "never visits bindings after main (unused = garbage is ignored)" do
        -- If the scanner kept reading past `main`, the lexer would crash
        -- on the garbage on the next line. Success means it stopped.
        n <- runFile "test/Fixtures/with_noise.hs"
        n `shouldBe` 7

    it "evaluates `1 + 2 + 3` → 6 (left-associative addition)" do
        n <- runFile "test/Fixtures/arith.hs"
        n `shouldBe` 6

    it "evaluates `2 + 3 * 4 - 1` → 13 (* binds tighter than + and -)" do
        n <- runFile "test/Fixtures/arith_precedence.hs"
        n `shouldBe` 13

    it "evaluates `100 - 10 - 5` → 85 (- is left-associative)" do
        n <- runFile "test/Fixtures/arith_assoc.hs"
        n `shouldBe` 85

    it "resolves cross-binding references: `main = a + b` → 42" do
        n <- runFile "test/Fixtures/cross.hs"
        n `shouldBe` 42

    it "resolves a 3-deep chain of bindings: main -> a -> b -> c = 41" do
        n <- runFile "test/Fixtures/chain.hs"
        n `shouldBe` 41

    it "ignores bindings not reachable from main (demand-driven)" do
        n <- runFile "test/Fixtures/cross_with_noise.hs"
        n `shouldBe` 99

    it "calls a 1-arg function with a literal: `inc 10` → 11" do
        n <- runFile "test/Fixtures/fn_inc.hs"
        n `shouldBe` 11

    it "mixes function calls with arithmetic: `double 5 + double 7 - 1` → 23" do
        n <- runFile "test/Fixtures/fn_double_mix.hs"
        n `shouldBe` 23

    it "passes a parameter as an argument to another function: `twice 10` → 22" do
        n <- runFile "test/Fixtures/fn_call_through_param.hs"
        n `shouldBe` 22
