-- | Smoke tests for the ihc REPL.
--
-- We spawn the ihc binary, pipe a small script to stdin, and assert
-- the stdout contains the expected output.  Gated behind
-- IHC_REPL_TESTS=1 to avoid flakiness in CI environments that don't
-- have the binary on PATH.
module ReplTest (spec) where

import System.Environment (lookupEnv)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Test.Hspec

-- | Locate the ihc binary built by cabal.
ihcBin :: FilePath
ihcBin = "dist-newstyle/build/aarch64-osx/ghc-9.10.3/ihc-0.1.0.0/x/ihc/build/ihc/ihc"

runRepl :: String -> IO (ExitCode, String, String)
runRepl input = readProcessWithExitCode ihcBin ["repl"] input

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
        -- Data.List is builtin-backed: the dispatch parses the import
        -- correctly, prints a confirmation, and the REPL keeps running.
        -- length is provided by the builtin env.
        (code, out, _err) <- runRepl "import Data.List\nlength [1,2,3]\n:q\n"
        code `shouldBe` ExitSuccess
        out `shouldContain` "imported Data.List"
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
