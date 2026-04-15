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
