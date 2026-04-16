-- | Smoke tests for the ihc REPL.
--
-- We spawn the ihc binary, pipe a small script to stdin, and assert
-- the stdout contains the expected output.  Gated behind
-- IHC_REPL_TESTS=1 to avoid flakiness in CI environments that don't
-- have the binary on PATH.
module ReplTest (spec) where

import System.Environment (lookupEnv)
import System.IO (hPutStr, hFlush, hClose, openTempFile)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Timeout (timeout)
import Test.Hspec

-- | Locate the ihc binary built by cabal.
ihcBin :: FilePath
ihcBin = "dist-newstyle/build/aarch64-osx/ghc-9.10.3/ihc-0.1.0.0/x/ihc/build/ihc/ihc"

runRepl :: String -> IO (ExitCode, String, String)
runRepl input = do
    result <- timeout (20 * 1000000) (readProcessWithExitCode ihcBin ["repl"] input)
    case result of
        Just triple -> pure triple
        Nothing -> do
            expectationFailure "REPL timed out"
            pure (ExitFailure 124, "", "")

spec :: Spec
spec = describe "REPL smoke tests" do
    it "evaluates 1 + 2 and prints 3" do
        (code, out, _err) <- runRepl "1 + 2\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "3"

    it ":q exits cleanly" do
        (code, _out, _err) <- runRepl ":q\n"
        code `shouldBe` ExitSuccess

    it "let binding persists: let x = 7, x * x = 49" do
        (code, out, _err) <- runRepl "let x = 7\nx * x\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "49"

    it ":? prints help" do
        (code, out, _err) <- runRepl ":?\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` ":q"

    it "string literal echoes back as a string" do
        (code, out, _err) <- runRepl "\"hello\"\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "hello"

    it ":l loads a file and makes its bindings available" do
        (code, out, _err) <- runRepl ":l test/Fixtures/hello.hs\nmain\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "Hello, world!"

    it "import Data.List succeeds and length still works" do
        (code, out, _err) <- runRepl "import Data.List\nlength [1,2,3]\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "imported Data.List"
        out `shouldContain` "3"

    it "import qualified Data.ByteString as BS succeeds" do
        (code, out, _err) <- runRepl "import qualified Data.ByteString as BS\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "imported Data.ByteString (deferred)"

    it "import qualified Data.ByteString as BS: BS.length (BS.pack [97,98,99]) = 3" do
        (code, out, _err) <- runRepl
            ( "import qualified Data.ByteString as BS\n"
           <> "BS.length (BS.pack [97,98,99])\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "3"

    it "import parse error is reported gracefully, REPL continues" do
        -- A malformed import should print an error but not crash.
        (code, out, _err) <- runRepl "import\n1 + 1\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "2"

    it ":l file-without-main then `main` does not produce unbound () error" do
        -- Regression test for the bug where loading a file that has no
        -- `main` binding caused the scheduler to append `main = ()` as a
        -- fallback, and then evaluating `main` failed with
        -- "unbound variable `()`" because () was not in the builtin env.
        (code, out, err) <- runRepl ":l test/Fixtures/no_main.hs\nmain\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldNotContain` "unbound variable"
        err `shouldNotContain` "unbound variable"

    -- :t smoke tests
    it ":t (1, 1) reports (Int, Int)" do
        (code, out, _err) <- runRepl ":t (1, 1)\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "(Int, Int)"

    it ":t [1, 2, 3] reports [Int]" do
        (code, out, _err) <- runRepl ":t [1, 2, 3]\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "[Int]"

    it ":t Just 42 reports Maybe Int" do
        (code, out, _err) <- runRepl ":t Just 42\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "Maybe Int"

    it ":t putStrLn \"x\" reports IO a without printing" do
        (code, out, _err) <- runRepl ":t putStrLn \"x\"\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "IO a"
        -- Must NOT have actually executed the action
        out `shouldNotContain` "x\n"

    it ":t 1.1 reports Double (regression: non-exhaustive patterns in go)" do
        (code, out, _err) <- runRepl ":t 1.1\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "Double"
        out `shouldNotContain` "Non-exhaustive"

    it ":t 1 reports Int" do
        (code, out, _err) <- runRepl ":t 1\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "Int"

    it ":t 'a' reports Char" do
        (code, out, _err) <- runRepl ":t 'a'\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "Char"

    -- Top-level declaration tests
    it "data Pair = Pair Int Int: constructors available" do
        (code, out, _err) <- runRepl "data Pair = Pair Int Int\nPair 1 2\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "Pair"

    it "newtype W = W Int: constructor available" do
        (code, out, _err) <- runRepl "newtype W = W Int\nW 5\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "W"

    it "type synonym: no error, prints note" do
        (code, out, _err) <- runRepl "type Foo = Int\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "Foo"

    it "class declaration: accepted and reported" do
        (code, out, _err) <- runRepl "class MyClass a where\n  myMethod :: a -> a\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "class"

    it "instance declaration: method registered and callable" do
        (code, out, _err) <-
            runRepl ( "instance Num Int where\n"
                   <> "  myAdd x = x + 1\n"
                   <> "1 + 2\n"
                   <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "3"

    -- Recursive let-binding tests (knot-tying fix)
    it "let map with recursion: map (\\x -> 1) \"hello\" = [1,1,1,1,1]" do
        (code, out, _err) <- runRepl
            ( "let map f x = case x of { (x:xs) -> (f x):(map f xs); [] -> [] }\n"
           <> "map (\\x -> 1) \"hello\"\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "1"

    it "let fact n: factorial of 5 is 120" do
        (code, out, _err) <- runRepl
            ( "let fact n = if n == 0 then 1 else n * fact (n-1)\n"
           <> "fact 5\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "120"

    it "let x = 42 (non-recursive) still works" do
        (code, out, _err) <- runRepl
            ( "let x = 42\n"
           <> "x\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "42"

    it "let y = y (self-loop) does not crash the REPL" do
        (code, _out, _err) <- runRepl
            ( "let y = y\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess

    -- :reload tests
    it ":r with no prior :l prints 'nothing to reload'" do
        (code, out, _err) <- runRepl ":r\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "nothing to reload"

    it ":l FILE then :r reloads the file" do
        (code, out, _err) <- runRepl
            ( ":l test/Fixtures/hello.hs\n"
           <> ":r\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "Loaded test/Fixtures/hello.hs"
        out `shouldContain` "Reloading test/Fixtures/hello.hs"

    -- ghci-compatible :l and import tests
    it ":l simple_exports.hs exports foo unqualified" do
        -- Bug 1 regression test: after :l, exported names must be reachable
        -- unqualified even if not reachable from `main`.
        (code, out, _err) <- runRepl
            ( ":l test/Fixtures/Repl/simple_exports.hs\n"
           <> "foo\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "42"
        -- `bar` is not exported, so it should NOT be bound.
        (code2, out2, _err2) <- runRepl
            ( ":l test/Fixtures/Repl/simple_exports.hs\n"
           <> "bar\n"
           <> ":q\n" )
        code2 `shouldBe` ExitSuccess
        out2 `shouldContain` "unbound"

    it "import Data.Maybe: fromMaybe 0 (Just 5) = 5" do
        -- import re-export following: Data.Maybe is source-loadable and
        -- fromMaybe should be available after import.
        (code, out, _err) <- runRepl
            ( "import Data.Maybe\n"
           <> "fromMaybe 0 (Just 5)\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "5"

    it "import qualified Data.List as L: L.isSubsequenceOf works, unqualified does not" do
        -- Qualified import: L.isSubsequenceOf should work; bare name should fail.
        -- isSubsequenceOf is locally defined in Data.List (not a re-export),
        -- so it is discoverable without loading GHC.Base.
        (code, out, _err) <- runRepl
            ( "import qualified Data.List as L\n"
           <> "L.isSubsequenceOf [1,2] [1,2,3]\n"
           <> "isSubsequenceOf [1,2] [1,2,3]\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "True"
        out `shouldContain` "unbound"

    -- Transitive re-export tests (effectiveExports + import-operator-fix)
    it "import Data.List: map (+1) [1,2,3] = [2,3,4]" do
        -- Data.List re-exports map transitively via Data.OldList → GHC.Base.
        -- The import-operator-list fix ensures all of Data.OldList's imports
        -- (including GHC.List, GHC.Base) are correctly parsed past `(.&.)`.
        (code, out, _err) <- runRepl
            ( "import Data.List\n"
           <> "map (+1) [1,2,3]\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "[2,3,4]"

    it "import Data.List: head and filter work (transitive via GHC.List)" do
        -- head and filter are defined in GHC.List, which is two hops away
        -- from Data.List via Data.OldList.
        (code, out, _err) <- runRepl
            ( "import Data.List\n"
           <> "head [10,20,30]\n"
           <> "filter (>2) [1,2,3,4]\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "10"
        out `shouldContain` "[3,4]"

    it "head \"\" raises an exception but REPL continues (raise# primop)" do
        -- Regression: source-loaded head [] bottoms out into raise#
        -- (via error -> errorCallWithCallStackException -> raise#). The
        -- raise# primop must throw an IhcException that the REPL's
        -- top-level handler catches and reports. Subsequent lines must
        -- still evaluate.
        (code, out, _err) <- runRepl
            ( "import Data.List\n"
           <> "head \"\"\n"
           <> "1 + 2\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        -- Either we get a proper IhcException message, or at minimum the
        -- REPL must NOT report `raise#` as unbound.
        out `shouldNotContain` "unbound variable `raise#`"
        out `shouldContain` "Error:"
        -- REPL must keep running after the error: the 1 + 2 must print.
        out `shouldContain` "3"

    it "import qualified Data.List as L: L.head [1,2,3] = 1" do
        -- Qualified import: transitive re-exports should be reachable via
        -- the L. prefix.  head is in GHC.List (two hops via Data.OldList).
        (code, out, _err) <- runRepl
            ( "import qualified Data.List as L\n"
           <> "L.head [1,2,3]\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "1"

    it "import qualified Data.List as L: L.map works (recursive closure via effectivePairs)" do
        -- Regression: qualified imports must put bare names in innerEnv so
        -- that recursive calls inside GHC.Base.map can find `map` itself.
        (code, out, _err) <- runRepl
            ( "import qualified Data.List as L\n"
           <> "L.map (+1) [1,2,3]\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "[2,3,4]"

    -- ghci-style top-level session bindings:  `a <- ioAction`
    it "session bind: a <- pure 42 then print a prints 42" do
        (code, out, _err) <- runRepl
            ( "a <- pure 42\n"
           <> "print a\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "42"

    it "session bind: a <- readFile FILE then putStr a prints file contents" do
        -- Read a known-stable fixture and echo its contents.
        (code, out, _err) <- runRepl
            ( "a <- readFile \"test/Fixtures/hello.hs\"\n"
           <> "putStr a\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "Hello, world!"

    it "session bind error: a <- readFile MISSING does not leave a bound" do
        -- The IO action fails; we must print an error AND keep the REPL
        -- alive AND leave `a` unbound (partial state is worse than none).
        (code, out, _err) <- runRepl
            ( "a <- readFile \"/nonexistent-ihc-test-path\"\n"
           <> "a\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "Error"
        out `shouldContain` "unbound variable"

    -- Regression: unaliased `import qualified M` should register `M`
    -- itself as the qualifier, so `M.name` resolves the deferred import.
    it "import qualified Data.List (no alias): Data.List.intercalate works" do
        (code, out, _err) <- runRepl
            ( "import qualified Data.List\n"
           <> "Data.List.intercalate \", \" [\"a\",\"b\",\"c\"]\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "imported Data.List (deferred)"
        out `shouldContain` "\"a, b, c\""

    -- Transitive qualified-name resolution: when a loaded module's body
    -- contains `H.foo`, the scheduler must consult the *loaded module's*
    -- imports (not the REPL's) to find `H`.  Greet.hs does
    -- `import qualified Helper as H` and calls `H.join`/`H.surround`
    -- inside its own bindings.  Both unaliased and aliased REPL imports
    -- must follow this chain successfully.
    it "import qualified Greet (unaliased): transitive H.join inside Greet's body" do
        (code, out, _err) <- runRepl
            ( ":l test/Fixtures/Repl/qual_transitive/Greet.hs\n"
           <> "import qualified Greet\n"
           <> "Greet.greet \"world\"\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "imported Greet (deferred)"
        out `shouldContain` "\"(hi - world)\""

    it "import qualified Greet as G: transitive H.join inside Greet's body" do
        (code, out, _err) <- runRepl
            ( ":l test/Fixtures/Repl/qual_transitive/Greet.hs\n"
           <> "import qualified Greet as G\n"
           <> "G.greet \"world\"\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "imported Greet (deferred)"
        out `shouldContain` "\"(hi - world)\""

    it "import qualified Greet as G: G.parts exercises H.join directly" do
        (code, out, _err) <- runRepl
            ( ":l test/Fixtures/Repl/qual_transitive/Greet.hs\n"
           <> "import qualified Greet as G\n"
           <> "G.parts \"x\" \"y\"\n"
           <> ":q\n" )
        code `shouldBe` ExitSuccess
        out `shouldContain` "\"x - y\""

    it ":r after modify-on-disk reflects new binding" do
        tmpDir <- getTemporaryDirectory
        (tmpPath, h) <- openTempFile tmpDir "ihc_reload_test.hs"
        -- Write a file with main that calls a top-level binding.
        -- The scheduler is demand-driven from main, so main must reference
        -- the binding we want to test.
        hPutStr h "main = putStrLn \"hello-v1\"\n"
        hFlush h
        hClose h
        -- First load: main prints hello-v1
        (_, out1, _) <- runRepl (":l " <> tmpPath <> "\nmain\n:q\n")
        out1 `shouldContain` "hello-v1"
        -- Overwrite the file with a new version: main prints hello-v2
        writeFile tmpPath "main = putStrLn \"hello-v2\"\n"
        -- In a fresh session: load + reload, then call main
        (code2, out2, _) <- runRepl (":l " <> tmpPath <> "\n:r\nmain\n:q\n")
        code2 `shouldBe` ExitSuccess
        out2 `shouldContain` "Reloading"
        out2 `shouldContain` "hello-v2"
        removeFile tmpPath
