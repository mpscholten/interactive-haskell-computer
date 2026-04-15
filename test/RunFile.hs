module RunFile (spec) where

import Control.Exception (bracket_)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO
import System.Directory (removeFile, getTemporaryDirectory)

import Test.Hspec

import IHC.Driver

-- | Run an IO action with stdout redirected to a temp file; return its
-- result and the captured stdout. Lets us verify that JIT'd programs
-- produce the expected output via host @base@'s 'putStrLn' / 'print'.
captureStdout :: IO a -> IO (a, String)
captureStdout action = do
    tmp <- getTemporaryDirectory
    (path, h) <- openTempFile tmp "ihc-test-stdout.txt"
    saved <- hDuplicate stdout
    hFlush stdout
    r <- bracket_
        (hDuplicateTo h stdout >> hClose h)
        (hDuplicateTo saved stdout >> hClose saved >> hFlush stdout)
        action
    out <- readFile path
    removeFile path
    pure (r, out)

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

    it "respects parens: `2 * (3 + 4)` → 14" do
        n <- runFile "test/Fixtures/parens.hs"
        n `shouldBe` 14

    it "if-then-else: takes the then-branch when condition is true" do
        n <- runFile "test/Fixtures/if_const.hs"
        n `shouldBe` 100

    it "if-then-else: takes the else-branch when condition is false" do
        n <- runFile "test/Fixtures/if_const_else.hs"
        n `shouldBe` 200

    it "self-recursion: factorial 10 → 3628800" do
        n <- runFile "test/Fixtures/fact.hs"
        n `shouldBe` 3628800

    it "self-recursion with two recursive calls: fib 10 → 55" do
        n <- runFile "test/Fixtures/fib.hs"
        n `shouldBe` 55

    it "parses multi-line if-then-else" do
        n <- runFile "test/Fixtures/multiline_if.hs"
        n `shouldBe` 100

    it "parses multi-line recursive body (fact 6)" do
        n <- runFile "test/Fixtures/multiline_fact.hs"
        n `shouldBe` 720

    it "2-arg function: `add2 3 4` → 7" do
        n <- runFile "test/Fixtures/fn_add2.hs"
        n `shouldBe` 7

    it "2-arg recursion: sumTo 1 10 → 55" do
        n <- runFile "test/Fixtures/fn_gcd.hs"
        n `shouldBe` 55

    it "2-arg with 1-arg args: `mul2 (inc 3) (inc 9)` → 40" do
        n <- runFile "test/Fixtures/fn_mixed.hs"
        n `shouldBe` 40

    it "calls host base putStrLn: prints `Hello, world!`" do
        (n, out) <- captureStdout (runFile "test/Fixtures/hello.hs")
        n   `shouldBe` 0
        out `shouldBe` "Hello, world!\n"

    it "calls host base print on Int: prints fib 15 = 610" do
        (n, out) <- captureStdout (runFile "test/Fixtures/print_fib.hs")
        n   `shouldBe` 0
        out `shouldBe` "610\n"

    it "all six relational operators yield 1 when their condition holds" do
        (n, out) <- captureStdout (runFile "test/Fixtures/cmp_ops.hs")
        n   `shouldBe` 0
        out `shouldBe` "1\n"

    it "&& and || combine relops correctly" do
        (n, out) <- captureStdout (runFile "test/Fixtures/and_or.hs")
        n   `shouldBe` 0
        out `shouldBe` "2\n"

    it "primality test (trial division) reports 97 as prime" do
        (n, out) <- captureStdout (runFile "test/Fixtures/primality.hs")
        n   `shouldBe` 0
        out `shouldBe` "1\n"

    it "unary minus on literal + abs builtin: -5 + abs (-3) = -2" do
        (n, out) <- captureStdout (runFile "test/Fixtures/negate_lit.hs")
        n   `shouldBe` 0
        out `shouldBe` "-2\n"

    it "do-block sequences three IO actions" do
        (n, out) <- captureStdout (runFile "test/Fixtures/do_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "first\nsecond\n12\n"

    it "do-block with computed values across recursion" do
        (n, out) <- captureStdout (runFile "test/Fixtures/do_with_recursion.hs")
        n   `shouldBe` 0
        out `shouldBe` "Computing fibonacci numbers:\n5\n55\n610\n"

    it "skips type signatures (parsed harmlessly): `fact 6` -> 720" do
        (n, out) <- captureStdout (runFile "test/Fixtures/typesig.hs")
        n   `shouldBe` 0
        out `shouldBe` "720\n"

    it "layout-aware do-block (no explicit braces)" do
        (n, out) <- captureStdout (runFile "test/Fixtures/do_layout.hs")
        n   `shouldBe` 0
        out `shouldBe` "Layout works!\n42\nAll three lines run.\n"

    it "real-style Haskell program (sigs, layout do, recursion, builtins)" do
        (n, out) <- captureStdout (runFile "test/Fixtures/realprogram.hs")
        n   `shouldBe` 0
        out `shouldBe`
            "=== math demo ===\n\
            \fact 10 =\n\
            \3628800\n\
            \fib 20 =\n\
            \6765\n\
            \is 97 prime?\n\
            \1\n\
            \is 100 prime?\n\
            \0\n\
            \done.\n"

    it "show + (++) + putStrLn produce composed messages" do
        (n, out) <- captureStdout (runFile "test/Fixtures/strings.hs")
        n   `shouldBe` 0
        out `shouldBe`
            "Hello, world! The answer is 42\n\
            \fib 10 = 55\n"

    it "3- and 4-argument functions: fma 3 4 5 = 17, ws 10 20 30 40 = 300" do
        (n, out) <- captureStdout (runFile "test/Fixtures/three_args.hs")
        n   `shouldBe` 0
        out `shouldBe` "17\n10\n300\n"

    it "let-in (single binding, used twice): `x*x + x` -> 110" do
        (n, out) <- captureStdout (runFile "test/Fixtures/let_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "110\n"

    it "nested let, inner references outer: `a*b` -> 30" do
        (n, out) <- captureStdout (runFile "test/Fixtures/let_nested.hs")
        n   `shouldBe` 0
        out `shouldBe` "30\n"

    it "let-in inside a function with params: `hyp 3 4` -> 25" do
        (n, out) <- captureStdout (runFile "test/Fixtures/let_with_param.hs")
        n   `shouldBe` 0
        out `shouldBe` "25\n"

    it "where clause with two value bindings: `hyp 3 4` -> 25" do
        (n, out) <- captureStdout (runFile "test/Fixtures/where_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "25\n"

    it "where with sequential dependencies: `quadratic 1 (-3) 2 5` -> 12" do
        (n, out) <- captureStdout (runFile "test/Fixtures/where_chain.hs")
        n   `shouldBe` 0
        out `shouldBe` "12\n21\n"

    it "case-of on Int: classify maps 0,1,2,_ to 0,1,1,99" do
        (n, out) <- captureStdout (runFile "test/Fixtures/case_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "0\n1\n1\n99\n"

    it "user ADT: Maybe + fromMaybe + pattern match on Just x" do
        (n, out) <- captureStdout (runFile "test/Fixtures/adt_maybe.hs")
        n   `shouldBe` 0
        out `shouldBe` "99\n42\n"

    it "user ADT: Tree + recursive size via Node _ l r" do
        -- size counts only Node constructors (Leaf -> 0). Node 1 (Node 2 Leaf Leaf) Leaf
        -- contains two Node constructors.
        (n, out) <- captureStdout (runFile "test/Fixtures/adt_tree.hs")
        n   `shouldBe` 0
        out `shouldBe` "2\n"

    it "user ADT: combined Maybe + Tree fixture" do
        (n, out) <- captureStdout (runFile "test/Fixtures/adt_combined.hs")
        n   `shouldBe` 0
        out `shouldBe` "99\n42\n2\n"

    -- Phase 2.2: lists, string literals as [Char], list patterns.
    it "list via explicit cons: 1 : 2 : 3 : [] prints as [1,2,3]" do
        (n, out) <- captureStdout (runFile "test/Fixtures/list_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "[1,2,3]\n"

    it "list-literal sugar: [1,2,3,4,5] prints as [1,2,3,4,5]" do
        (n, out) <- captureStdout (runFile "test/Fixtures/list_sugar.hs")
        n   `shouldBe` 0
        out `shouldBe` "[1,2,3,4,5]\n"

    it "list pattern match: sumList [1,2,3,4,5] = 15" do
        (n, out) <- captureStdout (runFile "test/Fixtures/list_pattern.hs")
        n   `shouldBe` 0
        out `shouldBe` "15\n"

    it "string is [Char]: putStrLn \"Hi\" prints Hi" do
        (n, out) <- captureStdout (runFile "test/Fixtures/string_as_chars.hs")
        n   `shouldBe` 0
        out `shouldBe` "Hi\n"

    it "take on a list: take 3 [10,20,30,40,50] = [10,20,30]" do
        (n, out) <- captureStdout (runFile "test/Fixtures/take_drop.hs")
        n   `shouldBe` 0
        out `shouldBe` "[10,20,30]\n"

    it "string (++) on [Char] lists concatenates and prints" do
        (n, out) <- captureStdout (runFile "test/Fixtures/string_concat.hs")
        n   `shouldBe` 0
        out `shouldBe` "Hello, world!\nanswer = 42\n"

    it "length on lists and strings" do
        (n, out) <- captureStdout (runFile "test/Fixtures/list_length.hs")
        n   `shouldBe` 0
        out `shouldBe` "5\n0\n5\n"

    it "char literal + putChar + escape" do
        (n, out) <- captureStdout (runFile "test/Fixtures/char_literal.hs")
        n   `shouldBe` 0
        out `shouldBe` "Hi\n"

    -- Phase 2.2.5: multi-clause function definitions and pattern guards.
    it "multi-clause on list patterns: isEmpty [] vs isEmpty (_:_)" do
        (n, out) <- captureStdout (runFile "test/Fixtures/multi_clause.hs")
        n   `shouldBe` 0
        out `shouldBe` "1\n0\n"

    it "multi-clause with multiple args: add 0 y = y; add x y = x + y" do
        (n, out) <- captureStdout (runFile "test/Fixtures/multi_clause_args.hs")
        n   `shouldBe` 0
        out `shouldBe` "10\n12\n"

    it "pattern guards: classify n by sign with `otherwise`" do
        (n, out) <- captureStdout (runFile "test/Fixtures/guards.hs")
        n   `shouldBe` 0
        out `shouldBe` "-1\n0\n1\n"

    it "pattern guards with `| True` fallback: sign 7 = 1" do
        (n, out) <- captureStdout (runFile "test/Fixtures/guards_with_patterns.hs")
        n   `shouldBe` 0
        out `shouldBe` "1\n"

    it "multi-clause recursion: len on [] and on 5-element list" do
        (n, out) <- captureStdout (runFile "test/Fixtures/multi_clause_recursive.hs")
        n   `shouldBe` 0
        out `shouldBe` "0\n5\n"

    it "multi-clause + guards combined: describe on several lists" do
        (n, out) <- captureStdout (runFile "test/Fixtures/multi_clause_guards.hs")
        n   `shouldBe` 0
        out `shouldBe` "0\n1\n-1\n2\n"
