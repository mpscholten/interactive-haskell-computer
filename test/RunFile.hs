module RunFile (spec) where

import Control.Exception (bracket_, displayException, try, SomeException)
import Control.Monad (forM_)
import Data.List (isInfixOf, sort, isSuffixOf)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.FilePath (takeDirectory, (</>))
import System.IO
import System.Directory (removeFile, getTemporaryDirectory, doesDirectoryExist, getHomeDirectory, listDirectory)
import System.Environment (lookupEnv)
import System.Timeout (timeout)

import Test.Hspec

import IHC.Classes (legacyHooks)
import IHC.Diagnostics (memDebugEnabled)
import IHC.Driver
import IHC.Eval (force)
import IHC.Parser (ParseError(..), defaultFixityTable, parseBodyExprWithFixity)
import IHC.Scan (BindingLhs(..), emptyKnownSymbols, findBinding)
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
    v             <- force legacyHooks mainT
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

expectFailureOrTimeout :: IO Int -> IO (Maybe SomeException)
expectFailureOrTimeout action = do
    r <- timeout (10 * 1000000) (try action)
    case r of
        Nothing           -> pure Nothing
        Just (Left e)     -> pure (Just e)
        Just (Right code) -> do
            expectationFailure
                ("expected a thrown exception or timeout for unsupported example; runFile returned "
                 <> show code)
            pure Nothing

spec :: Spec
spec = describe "Phase 1.0 — demand-driven single-pass JIT" do
    -- Regression: the scheduler used to leak state between consecutive
    -- runFile calls in the same process.  After the first call,
    -- 'globalLoadedModulesRef' was populated with ~150 modules; the
    -- second call's 'hydrateTransitiveImports' fanned that out to
    -- 222+, and 'buildAliases' / 'namesFromModule' for some
    -- transitively-pulled modules (e.g. Control.Monad.Trans.Except)
    -- spun indefinitely in ByteString 'compareBytes'.  The fix is to
    -- reset the cross-run state at the start of each
    -- 'loadProgramFromSource'.  This test exercises the path with
    -- *different* fixtures back-to-back so a future regression
    -- surfaces here rather than as a 1-hour-long hang in some random
    -- downstream `it` case.
    it "consecutive runFile calls with different fixtures don't hang" do
        n1 <- runFile "test/Fixtures/lit42.hs"
        n2 <- runFile "test/Fixtures/lit_large.hs"
        n3 <- runFile "test/Fixtures/with_noise.hs"
        n1 `shouldBe` 42
        n2 `shouldBe` 81985529216486895
        n3 `shouldBe` 7

    -- Flag-gated cross-fixture memory probe.  Inert in CI (pending
    -- unless IHC_MEM_DEBUG=1) so it does NOT shift the baseline
    -- pass/fail count, but runnable on demand to read the [ihc:mem]
    -- growth curve over a bounded 120-fixture in-process run without
    -- the ~45-min full suite:
    --   IHC_MEM_DEBUG=1 IHC_MEM_DEBUG_EVERY=10 \
    --     cabal run ihc-test -- --match "MEM: cross-fixture"
    -- Each runFile is wrapped in 'try' so a throwing fixture doesn't
    -- truncate the 120-sample curve.
    it "MEM: cross-fixture live-bytes probe over first 120 Coverage fixtures" $
        if not memDebugEnabled
            then pendingWith "set IHC_MEM_DEBUG=1 to run the memory probe"
            else do
                fs <- (take 120 . sort . filter (".hs" `isSuffixOf`))
                          <$> listDirectory "test/Fixtures/Coverage"
                forM_ fs $ \f -> do
                    _ <- try (runFile ("test/Fixtures/Coverage" </> f))
                            :: IO (Either SomeException Int)
                    pure ()

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

    it "module: `module Foo` re-export form — fn re-exported via Alias" do
        (n, out) <- captureStdout
            (runMainWithSiblings
                "test/Fixtures/Coverage/Modules/reexport_module_form/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "hi\n"

    it "module: ExportName re-export via unqualified import — Alias re-exports Inner.fn" do
        (n, out) <- captureStdout
            (runMainWithSiblings
                "test/Fixtures/Coverage/Modules/reexport_via_unqualified_import/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "hi\n"

    it "module: cross-module record update — defaultSettings { port = 8080 } where Settings is imported" do
        -- Regression for the Warp dry-run bug where 'rec { field = val }'
        -- on an imported record type silently exited 0 before subsequent
        -- IO actions could run, because the desugar saw an empty local
        -- 'FieldRegistry' and dropped the update.  Fixed end-to-end by
        -- 'visibleFieldRegistry' (imported fields visible at desugar)
        -- plus the loud runtime-error fallback added in 88384cb.
        (n, out) <- captureStdout
            (runMainWithSiblings
                "test/Fixtures/Coverage/Modules/warp_record_update_xmod/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "before\n8080\nafter\n"

    it "module: un-exported cross-module record field selector does not shadow Prelude.filter" do
        -- Regression for the warp hello-world startup crash: loading
        -- 'GHC.Event.KQueue' (Darwin's event backend, whose 'Event' record
        -- has a 'filter' field that KQueue does NOT export) registered a bare
        -- 'filter' accessor in the global field env, so unrelated modules'
        -- unqualified 'filter' resolved to the leaked accessor and died with
        -- "record accessor `filter` applied to non-constructor value".
        -- EventBackend owns 'filter' but doesn't export it; Main imports only
        -- the type. Exercises all three resolution paths: the owner's own
        -- field accessor (eventFilter -> 99), a NON-entry module using
        -- Prelude.filter (evens -> [2,4,6], the path warp actually hits via
        -- lazy buildSlotFromOwner), and the entry module's Prelude.filter
        -- ([1,3,5]). Fixed by gating bare field-selector accessors on export
        -- visibility ('exportedPublicFields') in loadProgramFromSource,
        -- buildSlotFromOwner, and tryGlobalFieldSlot.
        (n, out) <- captureStdout
            (runMainWithSiblings
                "test/Fixtures/Coverage/Modules/record_field_prelude_collision/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "99\n[2,4,6]\n[1,3,5]\n"

    it "module: PackageImports `import \"base\" Data.List (sort)` parses + runs" do
        (n, out) <- captureStdout
            (runFile "test/Fixtures/Modules/package_import.hs")
        n   `shouldBe` 0
        out `shouldBe` "[1,2,3]\n"

    it "module: export list `T(..)` exposes every constructor" do
        (n, out) <- captureStdout
            (runMainWithSiblings "test/Fixtures/Modules/ctor_reexport/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "Red\nGreen\nBlue\n"

    it "module: MagicHash ident in explicit import list doesn't bail the list" do
        -- Regression for the Data.Text.Show bug where `import GHC.Exts
        -- (Ptr(..), Int(..), Addr#, indexWord8OffAddr#)` hit a TkPrimId
        -- mid-list, silently truncated, and lost every subsequent
        -- `import qualified ... as X` on the same module. Downstream
        -- references to `X.name` then failed as UnresolvedName.
        (n, out) <- captureStdout
            (runMainWithSiblings
                "test/Fixtures/Modules/magichash_in_import_list/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "hello, ok\n"

    it "module: qualified class methods (P.negate, P.maxBound) in non-entry module" do
        (n, out) <- captureStdout
            (runMainWithSiblings
                "test/Fixtures/Modules/qualified_class_method/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "-42\n9223372036854775807\n"

    it "cache fallback: import Control.Monad.State from cached mtl (skip if not cached)" do
        -- Runs end-to-end only when BOTH (a) mtl-2.3.2 is available in
        -- the user's ihc source cache AND (b) IHC_MTL_CACHED_TEST=1 is
        -- set.  The fixture uses @let (v, s) = ...@ (tuple pattern in
        -- do-let) plus the whole mtl/transformers class graph which is
        -- slow to interpret from scratch; by default we skip to keep
        -- `cabal test ihc-test` fast and green.  Flip the env var to
        -- drive the slow cache-fallback path.
        home <- getHomeDirectory
        let mtlDir = home <> "/.cache/ihc/sources/mtl-2.3.2"
        cached <- doesDirectoryExist mtlDir
        mEnabled <- lookupEnv "IHC_MTL_CACHED_TEST"
        case (cached, mEnabled) of
            (False, _) -> pendingWith "mtl-2.3.2 not in ~/.cache/ihc/sources/ — skipping"
            (True, Nothing) -> pendingWith "slow mtl-from-source path; set IHC_MTL_CACHED_TEST=1 to run"
            (True, Just _)  -> do
                (n, out) <- captureStdout
                    (runMainWithSiblings
                        "test/Fixtures/Coverage/Modules/cached_mtl_import/Main.hs")
                n   `shouldBe` 0
                out `shouldBe` "0\n1\n"

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

    it "parser: bang operator in do-let RHS does not terminate layout" do
        src <- readSourceFile "test/Fixtures/Phase26/bang_infix_do_let.hs"
        known <- emptyKnownSymbols
        Just lhs <- findBinding src known "main"
        expr <- parseBodyExprWithFixity src defaultFixityTable (lhsClauses lhs)
        let rendered = show expr
        rendered `shouldSatisfy` isInfixOf "EVar \"!\""
        rendered `shouldSatisfy` isInfixOf "handle100Continue"

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

    it "CPP #include: template-include pattern — macros defined in wrapper expand in included body" do
        (n, out) <- captureStdout
            (runFile "test/Fixtures/CppInclude/template_include/main.hs")
        n   `shouldBe` 0
        out `shouldBe` "posix\nPosix\n'/'\n"

    it "CPP #include: missing file throws an exception mentioning the path" do
        result <- try (runFile "test/Fixtures/CppInclude/missing/main.hs")
                      :: IO (Either SomeException Int)
        let isSubstring needle haystack =
                any (needle ==) [take (length needle) (drop i haystack) | i <- [0..length haystack]]
        result `shouldSatisfy` \case
            Left e  -> isSubstring "no_such_file.hs" (show e)
            Right _ -> False

    --------------------------------------------------------------------
    -- OverloadedRecordDot
    --------------------------------------------------------------------
    it "record dot: p.name and p.age on a record-syntax ADT" do
        (n, out) <- captureStdout (runFile "test/Fixtures/RecordDot/basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "Alice\n30\n"

    it "record dot: chained x.a.b.c two-level access" do
        (n, out) <- captureStdout (runFile "test/Fixtures/RecordDot/chained.hs")
        n   `shouldBe` 0
        out `shouldBe` "Berlin\n"

    it "record dot: (.age) section used in map" do
        (n, out) <- captureStdout (runFile "test/Fixtures/RecordDot/section.hs")
        n   `shouldBe` 0
        out `shouldBe` "[30,25]\n"

    it "record dot: f . g with spaces is still composition (regression)" do
        (n, out) <- captureStdout (runFile "test/Fixtures/RecordDot/does_not_regress_compose.hs")
        n   `shouldBe` 0
        out `shouldBe` "10\n"

    it "record dot: p.name inside a larger expression (++ greet)" do
        (n, out) <- captureStdout (runFile "test/Fixtures/RecordDot/in_expr.hs")
        n   `shouldBe` 0
        out `shouldBe` "Hello, Alice!\n"

    --------------------------------------------------------------------
    -- Phase 3.6: ImplicitParams (?name + let ?x = …)
    --------------------------------------------------------------------
    it "implicit params: basic ?x usage via let ?x = 10 in foo 5" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase36/iparam_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "15\n"

    it "implicit params: lexical capture — closure captures ?x at definition, not call site" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase36/iparam_capture.hs")
        n   `shouldBe` 0
        out `shouldBe` "1\n"

    it "implicit params: nested let ?x shadows outer ?x" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase36/iparam_shadow.hs")
        n   `shouldBe` 0
        out `shouldBe` "2\n"

    it "implicit params: multiple implicit params ?x and ?y in same let" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase36/iparam_multiple.hs")
        n   `shouldBe` 0
        out `shouldBe` "30\n"

    it "implicit params: unbound ?x raises a runtime error" do
        result <- try (runFile "test/Fixtures/Phase36/iparam_unbound.hs")
                      :: IO (Either SomeException Int)
        result `shouldSatisfy` \case
            Left _  -> True
            Right _ -> False

    --------------------------------------------------------------------
    -- Phase 2.11: TH Lift-splice subset
    --------------------------------------------------------------------
    it "TH lift: $(lift (42 :: Int)) expands to literal 42" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase211/lift_int.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "TH lift: $(lift \"hello\") expands to string \"hello\"" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase211/lift_string.hs")
        n   `shouldBe` 0
        out `shouldBe` "hello\n"

    it "TH lift: $(lift [1,2,3]) expands to list [1,2,3]" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase211/lift_list.hs")
        n   `shouldBe` 0
        out `shouldBe` "[1,2,3]\n"

    it "TH lift: $(lift (1 :: Int, \"a\")) expands to a tuple" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase211/lift_tuple.hs")
        n   `shouldBe` 0
        out `shouldBe` "(1,\"a\")\n"

    it "TH lift: user-defined ADT Color auto-lifted via generic VCon path" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase211/lift_user_data.hs")
        n   `shouldBe` 0
        out `shouldBe` "Red\n"

    --------------------------------------------------------------------
    -- Phase 2.9.5: GADTs + Typeable/cast/Dynamic
    --------------------------------------------------------------------
    it "phase 2.9.5: GADT simple — MkInt constructor arity 1, getInt returns field" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase295/gadt_simple.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "phase 2.9.5: GADT arity — Pair2 :: Int -> Int -> Pair; pairSum works" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase295/gadt_arity.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "phase 2.9.5: GADT constrained — Show constraint adds dict arg, construction works" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase295/gadt_constrained.hs")
        n   `shouldBe` 0
        out `shouldBe` "constructed\n"

    it "phase 2.9.5: existential basic — forall a. Wrap a; arity 1, unwrap returns True" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase295/existential_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\n"

    it "phase 2.9.5: existential with constraint — forall a. Show a => MkShow a; arity 2" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase295/existential_with_constraint.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\n"

    it "phase 2.9.5: typeable_prim — toDyn/fromDynamic round-trip for Int" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase295/typeable_prim.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "phase 2.9.5: typeable_user — Dynamic works with user-defined data decl present" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase295/typeable_user.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "phase 2.9.5: cast_success — cast with same TypeRep returns Just" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase295/cast_success.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "phase 2.9.5: cast_fail — cast with different TypeRep returns Nothing" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase295/cast_fail.hs")
        n   `shouldBe` 0
        out `shouldBe` "Nothing\n"

    it "phase 2.9.5: dynamic_roundtrip — toDyn/fromDynamic with Int" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase295/dynamic_roundtrip.hs")
        n   `shouldBe` 0
        out `shouldBe` "99\n"

    --------------------------------------------------------------------
    -- Error message quality: parse errors carry file:line:col
    --------------------------------------------------------------------
    it "parse error shows file:line:col instead of raw byte offset" do
        result <- try (runFile "test/Fixtures/Coverage/parse_error_position_XFAIL.hs")
                      :: IO (Either ParseError Int)
        case result of
            Right _ -> expectationFailure "expected a ParseError but the file succeeded"
            Left err -> do
                -- displayException must format as  path:LINE:COL using
                -- the absolute file line (not a span-relative line), so the
                -- pointer in parse_error_position_XFAIL.hs lands on line 4
                -- (`       in x`) where the parser expected `=`.
                let msg = displayException err
                msg `shouldSatisfy` (":4:" `isInfixOf`)
                msg `shouldSatisfy` ("parse error at" `isInfixOf`)

    --------------------------------------------------------------------
    -- Phase 3.2 + 3.4: type families + DataKinds (parse-discard)
    --------------------------------------------------------------------
    it "type family: open type family + type instance declarations are parsed and discarded" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/type_family_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "123\n"

    it "type family: closed type family with where-block is parsed and discarded" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/type_family_closed.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "type family: associated type in class + instance is parsed and discarded" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/associated_type.hs")
        n   `shouldBe` 0
        out `shouldBe` "3\n"

    it "DataKinds: kind signatures (:: *) in type sigs are parsed and discarded" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/datakinds_kind_sig.hs")
        n   `shouldBe` 0
        out `shouldBe` "5\n"

    it "DataKinds: promoted constructor tick ('Nothing) in type sig is discarded" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/promoted_nothing.hs")
        n   `shouldBe` 0
        out `shouldBe` "99\n"

    it "IHP patterns: ModelSupport-style type families parse cleanly" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/ihp_patterns.hs")
        n   `shouldBe` 0
        out `shouldBe` "Hello, IHP!\n"

    it "IHP integration: full ModelSupport.Types-style file with all type-family patterns" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/ihp_integration.hs")
        n   `shouldBe` 0
        out `shouldBe` "IHP integration OK\n42\n"

    it "TF: GetTableName User reduces to \"users\" at runtime (IHP shape)" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/tf_get_table_name.hs")
        n   `shouldBe` 0
        out `shouldBe` "users\n"

    it "TF: PrimaryKey model closed-family constant RHS reduces" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/tf_primary_key.hs")
        n   `shouldBe` 0
        out `shouldBe` "id\n"

    it "TF: GetModelByTableName reverse-lookup closed family picks correct branch" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/tf_get_model_by_table_name.hs")
        n   `shouldBe` 0
        out `shouldBe` "User\nPost\n"

    it "TF: HeadSym cons-pattern on promoted Symbol list reduces" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/tf_include_nested.hs")
        n   `shouldBe` 0
        out `shouldBe` "hello\n"

    it "TF: FieldIndex (IHP-shape recursive Nat arithmetic) reduces" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/tf_nat_arith.hs")
        n   `shouldBe` 0
        out `shouldBe` "1\n"

    it "KnownSymbol dict: foo :: KnownSymbol s => Proxy s -> String; symbolVal p" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/known_symbol_dict.hs")
        n   `shouldBe` 0
        out `shouldBe` "hello\n"

    it "KnownNat dict: bar :: KnownNat n => Proxy n -> Integer; natVal p" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase32_34/known_nat_dict.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    --------------------------------------------------------------------
    -- Phase 3.5: OverloadedLabels
    --------------------------------------------------------------------
    it "Phase35: #email label prints as #email" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase35/label_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "#email\n"

    it "Phase35: label stored in let then printed" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase35/label_as_proxy.hs")
        n   `shouldBe` 0
        out `shouldBe` "#email\n"

    it "Phase35: label in tuple — IHP filterWhere pattern" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase35/label_in_tuple.hs")
        n   `shouldBe` 0
        out `shouldBe` "#email\nhi\n"

    it "Phase35: multiple labels in one function" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Phase35/label_multiple.hs")
        n   `shouldBe` 0
        out `shouldBe` "#firstName\n#lastName\n#email\n"

    --------------------------------------------------------------------
    -- A.1 Bang patterns: §3.17.2 strictness at let / lambda /
    -- constructor sub-pattern / do-bind sites.  Each fixture uses an
    -- IORef bump-counter inside a thunk to detect whether the bang
    -- forced eagerly: with strict bang the marker is non-zero by the
    -- time main reads it; without, it stays at zero.
    --------------------------------------------------------------------
    it "A.1 bang let: `let !x = e` in do-block forces eagerly" do
        (n, out) <- captureStdout (runFile "test/Fixtures/BangPatterns/let_force.hs")
        n   `shouldBe` 0
        out `shouldBe` "forced\n"

    it "A.1 bang lambda: `\\(!x) -> 0` forces argument on apply" do
        (n, out) <- captureStdout (runFile "test/Fixtures/BangPatterns/lambda_force.hs")
        n   `shouldBe` 0
        out `shouldBe` "forced\n"

    it "A.1 bang constructor sub-pattern: `f (MkT !y)` forces field" do
        (n, out) <- captureStdout (runFile "test/Fixtures/BangPatterns/con_force.hs")
        n   `shouldBe` 0
        out `shouldBe` "forced\n"

    it "A.1 bang do-bind: `!x <- m` forces bound result" do
        (n, out) <- captureStdout (runFile "test/Fixtures/BangPatterns/do_force.hs")
        n   `shouldBe` 0
        out `shouldBe` "forced\n"

    --------------------------------------------------------------------
    -- A.2 Irrefutable patterns: §3.17.3 lazy match.  `~p` matches
    -- every value; bound vars become thunks that re-attempt the match
    -- on force.  Without lazy semantics, `\ ~(Just x) -> 0` applied to
    -- Nothing crashes; with PIrref handled in matchPat the application
    -- returns 0 because x is never forced.
    --------------------------------------------------------------------
    it "A.2 lazy lambda: `\\ ~(Just x) -> 0` Nothing returns 0" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/IrrefutablePatterns/lazy_lambda.hs")
        n   `shouldBe` 0
        out `shouldBe` "0\n0\n"

    it "A.2 lazy case alt: `case _ of ~(Just x) -> 0` Nothing returns 0" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/IrrefutablePatterns/lazy_case.hs")
        n   `shouldBe` 0
        out `shouldBe` "0\n0\n"

    it "A.2 deferred failure: forcing a var bound by ~(Just x) on Nothing raises" do
        -- Two acceptable shapes prove the deferred match fires:
        --
        --   (a) The host @ErrorCall@ from the irrefutable-pattern check
        --       escapes through 'runFile'.  This was the original
        --       behaviour, when the program's @try \@SomeException@
        --       did not bridge the host exception.
        --
        --   (b) The program's own @try@ catches the @ErrorCall@ — the
        --       exception bridge now routes deferred-match failures
        --       through 'try' (see e.g. @ee1f498@ "VIO/IO tag bridge,
        --       SomeException wrap") — so the program runs to
        --       completion, prints exactly @"deferred\n"@, and
        --       'runFile' returns 0.
        --
        -- Either shape proves the deferred-match semantics: forcing
        -- the bound var on @Nothing@ DID fire the error.  The wrong
        -- outcome would be @runFile@ returning 0 with stdout
        -- @"not deferred (got N)\n"@ (the deferred match silently
        -- succeeded) — that we still reject.
        r <- try (captureStdout
                     (runFile "test/Fixtures/IrrefutablePatterns/lazy_force_failure.hs"))
        case (r :: Either SomeException (Int, String)) of
            Left e -> do
                let msg = displayException e
                msg `shouldSatisfy` (\m -> "Irrefutable pattern failed" `isInfixOf` m)
            Right (code, out) -> do
                code `shouldBe` 0
                out  `shouldBe` "deferred\n"

    --------------------------------------------------------------------
    -- A.3 Numeric literals: parse out-of-Int64-range integers as
    -- 'LInteger Integer' and evaluate to 'VInteger' instead of silently
    -- truncating via host 'fromInteger'.
    --------------------------------------------------------------------
    it "A.3 big-Integer literal: source decimal preserved through print" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/NumLiterals/big_integer_literal.hs")
        n   `shouldBe` 0
        out `shouldBe` "12345678901234567890123456789\n100000000000000000000\n"

    --------------------------------------------------------------------
    -- A.5 Strict data-constructor fields (Report §4.2.1).  `data T =
    -- MkT !Int Int` forces the strict field on construction.  Detection
    -- via an IORef bumped from inside the strict-field thunk — with
    -- the bang honored, the marker is exactly 1; without, it's 0.
    --------------------------------------------------------------------
    it "A.5 strict field forces on construction" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/StrictFields/strict_field_force.hs")
        n   `shouldBe` 0
        out `shouldBe` "forced\n"

    --------------------------------------------------------------------
    -- B.1 Superclass dictionaries: scanner captures the head context
    -- (`class C a => D a where …`) into a global superclass-relation
    -- map; the debug builtin `__ihc_class_supers` exposes it.  This
    -- is the data layer for B.2 (default methods using superclass
    -- methods) and the deferred coherence check.
    --------------------------------------------------------------------
    it "B.1 scanner captures class superclass context" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/Superclass/superclass_capture.hs")
        n   `shouldBe` 0
        out `shouldBe`
            "MyEq: []\n\
            \MyOrd: [\"MyEq\"]\n\
            \MyHashable: [\"MyEq\",\"MyShow\"]\n"

    --------------------------------------------------------------------
    -- B.2 Default methods (Report §4.3.2): mutually-recursive class
    -- defaults `eq = not . neq` / `neq = not . eq` — instance defines
    -- only one direction, the other dispatches through the default.
    --------------------------------------------------------------------
    it "B.2 mutually-recursive class defaults route through dispatcher" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/DefaultMethods/mutual_default.hs")
        n   `shouldBe` 0
        out `shouldBe` "False\nTrue\n"

    --------------------------------------------------------------------
    -- B.3 Overlap pragmas (GHC user guide §6.8.7) — DEFERRED.
    -- ihc's lexer consumes @{-# OVERLAPPING #-}@ etc. as whitespace
    -- and the registry has no notion of specificity (tags can't
    -- distinguish @[Char]@ from @[a]@). Real overlap-aware dispatch
    -- depends on the typed-IR slice (C.2). This fixture is a
    -- regression guard for the pragma-tolerance level — declarations
    -- with overlap pragmas must continue to parse and run.
    --------------------------------------------------------------------
    it "B.3 overlap pragmas are tolerated (last-write-wins today)" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/Overlap/overlap_tolerated.hs")
        n   `shouldBe` 0
        out `shouldBe` "1\n"

    --------------------------------------------------------------------
    -- A.4 Numeric defaulting (Report §4.3.4) — DEFERRED.
    -- ihc monomorphises every integer literal at parse time (A.3
    -- minimum scope), so there's no ambiguous Num constraint for
    -- the defaulting rule to act on.  The full rule depends on the
    -- elaborator-driven 'fromInteger'-insertion path that A.3
    -- deferred.  This fixture is a tolerance guard: a top-level
    -- 'default (Int, Double)' declaration must not break parsing.
    --------------------------------------------------------------------
    it "A.4 top-level `default (...)` is tolerated" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/Defaulting/default_decl_tolerated.hs")
        n   `shouldBe` 0
        out `shouldBe` "3\n"

    --------------------------------------------------------------------
    -- B.4 Functional dependencies (GHC user guide §6.8.8) — DEFERRED
    -- improvement.  The parser/scanner tolerate `class C a b | a -> b`
    -- cleanly (the `|` clause is skipped without affecting class-name
    -- capture or method registration) and single-arg dispatch works.
    -- Constraint improvement via fundeps depends on the typed-IR
    -- slice (C.2).
    --------------------------------------------------------------------
    it "B.4 FunDep-using class parses + dispatches via head type" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/FunDeps/fundep_tolerated.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nFalse\n"

    --------------------------------------------------------------------
    -- B.5a Quantified constraints (GHC user guide §6.8.10) — DEFERRED
    -- solver.  ihc's parseClassHead handles the @(forall a. C a => D
    -- (f a))@ syntax via the depth-aware token scan from B.1, and
    -- runtime tag-keyed dispatch is unaffected by the type-level
    -- quantifier, so programs that use @QuantifiedConstraints@ load
    -- and run.  Skolemise/discharge/regeneralise solving (B.5b) ships
    -- after the elaborator-integrated lowering (C.2.3 follow-up).
    --------------------------------------------------------------------
    it "B.5a quantified-constraint class parses + dispatches" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/QuantifiedConstraints/qc_tolerated.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\nFalse\n"

    --------------------------------------------------------------------
    -- C.1 GADT pattern-match refinement (Haskell Report addendum /
    -- GHC user guide §6.4.7) — DEFERRED.  ihc is type-permissive,
    -- so the GADT-form syntax + 'coerce Refl x = x' shape works at
    -- runtime without any refinement substitution.  Real elaborator
    -- work to threadType refinement through the case-alt body
    -- ships after the typed-IR (C.2.3 follow-up).
    --------------------------------------------------------------------
    it "C.1 GADT-form data + Refl pattern-match runtime path" do
        (n, out) <- captureStdout
                       (runFile "test/Fixtures/GADTs/gadt_refl_runtime.hs")
        n   `shouldBe` 0
        out `shouldBe` "5\n'a'\n"

    --------------------------------------------------------------------
    -- Graduated XFAILs (fixtures that now pass)
    --------------------------------------------------------------------
    it "bang pattern strict: sumStrict with [1..10] = 55" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Coverage/bang_pattern_strict.hs")
        n   `shouldBe` 0
        out `shouldBe` "55\n"

    it "ops fixity prec: ^ operator and mixed arith" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Coverage/ops_fixity_prec.hs")
        n   `shouldBe` 0
        out `shouldBe` "7\n26\n5\n512\n"

    it "io file roundtrip: writeFile then readFile" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Coverage/io_file_roundtrip.hs")
        n   `shouldBe` 0
        out `shouldBe` "hello from ihc\n"

    it "typeapp promoted: @'True type application is parsed and ignored" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Coverage/typeapp_promoted.hs")
        n   `shouldBe` 0
        out `shouldBe` "True\n"

    it "runST basic: runST (return 42) evaluates correctly" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Coverage/runst_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"

    it "runST STRef counter: source-loaded ST actions sequence correctly" do
        (n, out) <- captureStdout (runFile "test/Fixtures/Coverage/sem_runst_basic.hs")
        n   `shouldBe` 0
        out `shouldBe` "10\n"

    --------------------------------------------------------------------
    -- QuickWins: small GHC2021/common extensions (IHP Tier-3)
    --------------------------------------------------------------------
    it "NamedFieldPuns: Foo { x, y } in construction and pattern" do
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/named_field_puns.hs")
        n   `shouldBe` 0
        out `shouldBe` "3\n"

    it "NamedFieldPuns: postfix record-update r { x } uses in-scope x" do
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/named_field_puns_update.hs")
        n   `shouldBe` 0
        out `shouldBe` "101\n"

    it "NamedFieldPuns: mixing pun + explicit field = value in one record" do
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/named_field_puns_mixed.hs")
        n   `shouldBe` 0
        out `shouldBe` "60\n"

    it "RecordWildCards: Foo {..} in pattern binds fields, in expression reads them" do
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/record_wildcards.hs")
        n   `shouldBe` 0
        out `shouldBe` "3\n30\n"

    it "Lazy pattern: let ~(x, y) = ... binds through the tuple" do
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/lazy_pat.hs")
        n   `shouldBe` 0
        out `shouldBe` "3\n"

    it "Lazy pattern: \\ ~(a, b) -> ... in a lambda argument" do
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/lazy_pat_lambda.hs")
        n   `shouldBe` 0
        out `shouldBe` "30\n"

    it "Either: Left/Right registered in builtin env regardless of Data.Either source-load" do
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/either_builtin.hs")
        n   `shouldBe` 0
        out `shouldBe` "Right 42\nLeft \"oops\"\n42\n"

    it "Data.Text.IO.putStrLn source-loads (no host shim for hPutStreamOrUtf8)" do
        -- Regression test for the ihp-smoke-probe finding #3: when an
        -- earlier probe loaded Data.Text.IO via IHP.Prelude it blew up on
        -- `unbound variable hPutStreamOrUtf8`. Source-loading improvements
        -- made the symbol resolve cleanly, so the "add a host builtin"
        -- mitigation is no longer needed. Pin that we can reach the
        -- Data.Text.IO output path without host shims.
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/text_io.hs")
        n   `shouldBe` 0
        out `shouldBe` "hello text\n"

    it "UserInfixOp: (|>) defined via section form `x |> f = f x`" do
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/user_infix_operator.hs")
        n   `shouldBe` 0
        out `shouldBe` "6\n"

    it "UserInfixOp: (|>) defined via prefix form `(|>) x f = f x`" do
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/user_infix_operator_prefix.hs")
        n   `shouldBe` 0
        out `shouldBe` "6\n"

    it "UserInfixOp: (|>) imported from another module" do
        (n, out) <- captureStdout
            (runMainWithSiblings "test/Fixtures/QuickWins/user_op_module/Main.hs")
        n   `shouldBe` 0
        out `shouldBe` "6\n"

    it "IsLabel dispatch: default Proxy instance + pattern-match transparency" do
        (n, out) <- captureStdout
            (runFile "test/Fixtures/QuickWins/islabel_dispatch.hs")
        n   `shouldBe` 0
        out `shouldBe` "#email\nfromLabel-yielded Proxy matched\nraw VLabel matched Proxy\n"

    it "TypeApplications: value-level @T parses and evaluator ignores" do
        -- Proxy @\"email\" and id' @Int both keep their inner semantics
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/value_level_tyapp.hs")
        n   `shouldBe` 0
        out `shouldBe` "Proxy\n42\n"

    it "TypeApplications: chained @T @T on a user function" do
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/value_level_tyapp_chained.hs")
        n   `shouldBe` 0
        out `shouldBe` "1\nJust 7\n"

    it "DataKinds: symbolVal / natVal recover lifted literals at runtime" do
        -- Covers both the @(Proxy :: Proxy "foo")@ annotation form and
        -- the @(Proxy @"foo")@ TypeApplications form, for Symbol and Nat
        -- kinds.
        (n, out) <- captureStdout (runFile "test/Fixtures/QuickWins/symbol_val.hs")
        n   `shouldBe` 0
        out `shouldBe` "hello\nworld\n42\n99\n"

    it "AllowAmbiguousTypes: pragma is a no-op since ihc skips type checking" do
        -- Regression for the IHP unsupported-scan: IHP uses this pragma
        -- in ~55 files to allow signatures whose type variables only
        -- appear in constraints (picked at use sites via
        -- TypeApplications).  ihc doesn't enforce ambiguity checks, so
        -- the pragma is parse-and-discard; the actual mechanism
        -- (value-level @T + symbolVal) already works.
        (n, out) <- captureStdout
            (runFile "test/Fixtures/QuickWins/allow_ambiguous_types.hs")
        n   `shouldBe` 0
        out `shouldBe` "42\n"


    it "IsLabel dispatch: user-defined instance overrides default Proxy" do
        (n, out) <- captureStdout
            (runFile "test/Fixtures/QuickWins/islabel_user_instance.hs")
        n   `shouldBe` 0
        out `shouldBe` "Wrap \"custom\"\n"

    it "IsLabel dispatch: IHP-shaped `IsLabel \"field\" T` with Symbol literal" do
        -- Each Symbol-keyed instance must select its own fromLabel body
        -- rather than colliding on a single (class, Type) registry slot.
        (n, out) <- captureStdout
            (runFile "test/Fixtures/QuickWins/islabel_symbol_dispatch.hs")
        n   `shouldBe` 0
        out `shouldBe` "Wrap \"from-email\"\nWrap \"from-name\"\n"

    it "error_message: head [] surfaces the real `Prelude.head: empty list` payload" do
        -- Source-loaded error + raise# path must produce a message
        -- containing the real payload string rather than the fallback
        -- unbound-variable form.  See Scheduler.buildBaseEnv and
        -- Builtins.forceToException for the interacting machinery.
        r <- try (runFile "test/Fixtures/QuickWins/error_message.hs")
        case (r :: Either SomeException Int) of
            Right code -> expectationFailure
                ("expected non-zero exit / thrown exception; runFile returned "
                 <> show code)
            Left e -> do
                let msg = displayException e
                msg `shouldSatisfy` (\m -> "head" `isInfixOf` m)
                msg `shouldSatisfy` (\m -> "empty list" `isInfixOf` m)
                msg `shouldNotSatisfy`
                    (\m -> "unbound variable `errorCallWithCallStackException`"
                           `isInfixOf` m)

    --------------------------------------------------------------------
    -- HSX + Blaze hello-world smoke fixtures (expected-fail).
    --
    -- These record the target for the HSX rendering milestone. Both
    -- examples throw today; the tests assert the current error
    -- messages so the suite stays green and the errors changing
    -- signals real progress. Graduate to positive expectations once
    -- rendering works end-to-end.
    --------------------------------------------------------------------
    it "examples/hsx_hello: [hsx|...|] QuasiQuoter is not expanded (expected-fail)" do
        -- Pins "HSX quasi-quoting still doesn't work" without
        -- pinning the exact error message: the failure mode shifts
        -- as plumbing improves, and a literal-string match would
        -- make every shift look like a regression.
        --
        -- Recent failure modes seen in this slot:
        --
        -- - 'IHC.Eval.applyIP: not a function: <IO> applied to <State…>'
        --   (parser body returned VIO where source expected Identity).
        -- - 'class-method dispatch: no instance of `MonadParsec`
        --   for type `Just` (method `takeWhileP`)'
        --   (Stage 4 of lazy registration: Megaparsec is no longer
        --   eagerly loaded; without an instance directory the
        --   dispatcher can't find the ParsecT instance).
        --
        -- Retarget to a positive expectation once HSX rendering
        -- works end-to-end.
        _ <- expectFailureOrTimeout
            (runMainWithSiblings "examples/hsx_hello/Main.hs")
        pure ()

    it "examples/blaze_hello: blaze-html rendering path errors today (expected-fail)" do
        -- With the HSX/blaze source cache populated, this reaches the
        -- renderer and currently fails in the chunk-concatenation path
        -- ('concatMap: not a list: ...').  On a fresh dev machine where
        -- scripts/cache-hsx-deps.sh has not been run, the legitimate
        -- earlier blocker is the missing source-loaded renderer binding.
        mErr <- expectFailureOrTimeout
            (runMainWithSiblings "examples/blaze_hello/Main.hs")
        case mErr of
            Nothing -> pure ()
            Just e -> do
                let msg = displayException e
                msg `shouldSatisfy`
                    (\m -> "concatMap: not a list" `isInfixOf` m
                        || "unbound variable `Text.Blaze.Html.Renderer.String.renderHtml`" `isInfixOf` m
                        || "unbound variable `H.toHtml`" `isInfixOf` m
                        || "<ihc-method-placeholder>:ToMarkup/toMarkup" `isInfixOf` m
                        || "<>: no Semigroup instance registered for type `Char`" `isInfixOf` m)
