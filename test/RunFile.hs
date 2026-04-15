module RunFile (spec) where

import Control.Exception (bracket_, try, SomeException)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.FilePath (takeDirectory)
import System.IO
import System.Directory (removeFile, getTemporaryDirectory)

import Test.Hspec

import IHC.Driver
import IHC.Eval (force)
import IHC.Scheduler (loadProgramFromSource)
import IHC.Source (readSourceFile)
import IHC.Val (Val(..))

-- | Phase-2.5 multi-file entry point. Equivalent to 'runFile' but with
-- an explicit search path so imports like @import Foo@ can resolve to
-- sibling files on disk. The regular single-file 'runFile' will gain a
-- search-path arg in a follow-up Driver patch; until then the tests
-- use this helper directly.
runFileWithSearch :: [FilePath] -> FilePath -> IO Int
runFileWithSearch searchPath path = do
    src <- readSourceFile path
    (_env, mainT) <- loadProgramFromSource searchPath src
    v             <- force mainT
    final         <- runIO v
    case final of
        VInt n -> pure (fromIntegral n)
        _      -> pure 0
  where
    runIO (VIO io) = io >>= runIO
    runIO x        = pure x

-- | Convenience: infer the search path from the Main.hs location.
runMainWithSiblings :: FilePath -> IO Int
runMainWithSiblings path = runFileWithSearch [takeDirectory path] path

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

    it "all six relational operators return True for their conditions" do
        (n, out) <- captureStdout (runFile "test/Fixtures/cmp_ops.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nTrue\nTrue\nTrue\nTrue\nTrue\n"

    it "&& and || combine relops correctly" do
        (n, out) <- captureStdout (runFile "test/Fixtures/and_or.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nFalse\nTrue\nFalse\n"

    it "primality test (trial division) reports 97 as prime" do
        (n, out) <- captureStdout (runFile "test/Fixtures/primality.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nFalse\n"

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

    -- Phase 2.4: IO monad + primops.
    it "io: do-block bind of `return 42` then print" do
        (n, out) <- captureStdout (runFile "test/Fixtures/io_return.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "io: `let x = 100` inside a do-block scopes to the rest of the block" do
        (n, out) <- captureStdout (runFile "test/Fixtures/io_let_in_do.hs")
        n   `shouldBe` 0
        out `shouldBe` "100\n"

    it "io: two binds in one do-block compose (a + b = 3)" do
        (n, out) <- captureStdout (runFile "test/Fixtures/io_nested_bind.hs")
        n   `shouldBe` 0
        out `shouldBe` "3\n"

    it "io: newIORef + readIORef round-trips a value" do
        (n, out) <- captureStdout (runFile "test/Fixtures/io_ioref_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "99\n"

    it "io: `seq (1 + 1) 42` forces LHS then returns RHS" do
        (n, out) <- captureStdout (runFile "test/Fixtures/io_seq.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "io: ord 'A' = 65; chr 65 = 'A'" do
        (n, out) <- captureStdout (runFile "test/Fixtures/io_ord_chr.hs")
        n   `shouldBe` 0
        out `shouldBe` "65\n'A'\n"

    --------------------------------------------------------------------
    -- Phase 2.5: modules + imports.
    --------------------------------------------------------------------
    it "module: simple unqualified import (Bar.greet + Bar.suffix)" do
        (n, out) <- captureStdout
            (runMainWithSiblings "test/Fixtures/Modules/simple/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "Hello, world!\nAside from Bar.\n"

    it "module: `import qualified Bar as B` + `import Foo (greet)`" do
        (n, out) <- captureStdout
            (runMainWithSiblings "test/Fixtures/Modules/qualified/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "Hello, world!\nAside from Bar.\n"

    it "module: export list — only exported names are reachable" do
        (n, out) <- captureStdout
            (runMainWithSiblings "test/Fixtures/Modules/export_list/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "Hello, friend!\n"

    it "module: three-level import chain Main -> Foo -> Bar" do
        (n, out) <- captureStdout
            (runMainWithSiblings "test/Fixtures/Modules/transitive/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "Hi, world!\n"

    --------------------------------------------------------------------
    -- Phase 2.6: language extensions + hand-rolled CPP.
    --------------------------------------------------------------------
    it "multi-arg lambda: (\\x y z -> x * y + z) 2 3 4 = 10" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/lambda_multiarg.hs")
        n   `shouldBe` 0
        out `shouldBe` "10\n"

    it "sections: (+ 1) 41 = 42, subtract 1 applied yields 42" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/sections.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n42\n"

    it "backtick infix: 10 `div` 3 = 3" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/backtick.hs")
        n   `shouldBe` 0
        out `shouldBe` "3\n"

    it "`$` as infix application: print $ 1 + 2 * 3 = 7" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/dollar.hs")
        n   `shouldBe` 0
        out `shouldBe` "7\n"

    it "2-tuple literal + tuple pattern: swap (1,2) = (2,1)" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/tuple.hs")
        n   `shouldBe` 0
        out `shouldBe` "(2,1)\n"

    it "3-tuple literal + tuple pattern: rotate (1,2,3) = (2,3,1)" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/tuple3.hs")
        n   `shouldBe` 0
        out `shouldBe` "(2,3,1)\n"

    it "as-pattern: firstAndAll [1,2,3] = (1,[1,2,3])" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/as_pattern.hs")
        n   `shouldBe` 0
        out `shouldBe` "(1,[1,2,3])\n"

    it "LambdaCase: classifies 0 as \"zero\" and 5 as \"other\"" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/lambda_case.hs")
        n   `shouldBe` 0
        out `shouldBe` "zero\nother\n"

    it "multi-way if: sgn (-3) = -1" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/multiway_if.hs")
        n   `shouldBe` 0
        out `shouldBe` "-1\n"

    it "fixity decl: infixl `myop` drives left-associative chain" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/fixity_decl.hs")
        n   `shouldBe` 0
        out `shouldBe` "123\n"

    it "CPP: #ifdef __GLASGOW_HASKELL__ selects the ghc branch" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/cpp_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "ghc-flavoured\n"

    it "bang pattern: !x !y parses and runs" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/bang_pattern.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "composition `.` via Pratt parser: ((*2) . (+3)) 4 = 14" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase26/compose.hs")
        n   `shouldBe` 0
        out `shouldBe` "14\n"

    --------------------------------------------------------------------
    -- Phase 2.3: type classes (dictionary passing).
    --------------------------------------------------------------------
    it "class: Eq Int — == and /= return proper Bool" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase23/class_eq_int.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nFalse\nTrue\nFalse\n"

    it "class: Ord Int — <, <=, >, >= return proper Bool" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase23/class_ord_int.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nFalse\nTrue\nTrue\nTrue\nFalse\n"

    it "class: Show dispatch — show Int, Bool, Char" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase23/class_show_int.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\nTrue\nFalse\n'x'\n"

    it "class: Eq list — structural equality on [Int] and String" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase23/class_eq_list.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nFalse\nTrue\nTrue\nFalse\n"

    it "class: Eq Maybe — structural equality on Maybe Int" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase23/class_eq_maybe.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nFalse\nTrue\nFalse\n"

    it "class: Bool operations return proper Bool constructors" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase23/class_bool_ops.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nFalse\nTrue\nFalse\nFalse\nTrue\n"

    it "class: user-defined Eq instance on ADT Color" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase23/class_user_defined.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nFalse\nTrue\nFalse\n"

    it "class: user-defined Show instance on ADT Shape" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase23/class_show_custom.hs")
        n   `shouldBe` 0
        out `shouldBe` "Circle\nSquare\nTriangle\n"

    --------------------------------------------------------------------
    -- Phase 2.8: ByteArray#/ForeignPtr/Word8/Storable + GHC.Exts primops
    --------------------------------------------------------------------
    it "phase 2.8: bit ops (.&., .|., xor, shiftL, shiftR)" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase28/primop_bits.hs")
        n   `shouldBe` 0
        out `shouldBe` "8\n14\n6\n-1\n16\n4\n8\n"

    it "phase 2.8: quot, rem, div, divMod, quotRem" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase28/primop_quot_rem.hs")
        n   `shouldBe` 0
        out `shouldBe` "3\n2\n3\n3\n2\n3\n2\n"

    it "phase 2.8: unsafePerformIO runs IO synchronously" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase28/primop_unsafe_perform_io.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "phase 2.8: runRW# applies function to RealWorld and extracts result" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase28/primop_run_rw.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "phase 2.8: mallocForeignPtrBytes + poke + peek round-trip" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase28/primop_malloc_foreignptr.hs")
        n   `shouldBe` 0
        out `shouldBe` "65\n"

    --------------------------------------------------------------------
    -- CPP #include directive
    --------------------------------------------------------------------
    it "CPP #include: simple include splices helper bindings inline" do
        (n, out) <- captureStdout
            (runFile "test/Fixtures/CppInclude/simple/main.hs")
        n   `shouldBe` 0
        out `shouldBe` "hello from helper\n"

    it "CPP #include: #define from outer file propagates into included file" do
        (n, out) <- captureStdout
            (runFile "test/Fixtures/CppInclude/shared_define/main.hs")
        n   `shouldBe` 0
        out `shouldBe` "greetings enabled\n"

    it "CPP #include: nested includes (main -> a.hs -> b.hs) work correctly" do
        (n, out) <- captureStdout
            (runFile "test/Fixtures/CppInclude/nested/main.hs")
        n   `shouldBe` 0
        out `shouldBe` "100\n"

    it "CPP #include: missing file throws an exception mentioning the path" do
        result <- try (runFile "test/Fixtures/CppInclude/missing/main.hs")
                      :: IO (Either SomeException Int)
        result `shouldSatisfy` \case
            Left e  -> "no_such_file.hs" `isSubstring` show e
            Right _ -> False
      where
        isSubstring needle haystack =
            any (needle ==) [take (length needle) (drop i haystack) | i <- [0..length haystack]]
