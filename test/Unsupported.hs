-- | Auto-discovered pending suite.  Each fixture under
-- @test/Fixtures/Unsupported/@ documents a real Hackage-package gap
-- (parser, language extension, or evaluator).  Every test is emitted
-- as @pendingWith <gap line>@ so the hspec output names the gap and
-- the fixture serves as a ready-to-graduate repro.
--
-- To graduate a fixture when the underlying gap is closed:
--
--   1. Remove the @-- Gap:@ line from the fixture (or just keep it);
--   2. In this module replace @pendingWith@ with a real expectation,
--      e.g. @runFile path >>= (`shouldBe` 0)@;
--   3. Or move the fixture to @test/Fixtures/Coverage/@ where the
--      auto-discovery in "Coverage" will pick it up with golden-output
--      support.
module Unsupported (spec) where

import Data.List (isPrefixOf, isSuffixOf, sort)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeBaseName)

import Test.Hspec

fixtureDir :: FilePath
fixtureDir = "test/Fixtures/Unsupported"

spec :: Spec
spec = describe "Unsupported — features used by Hackage packages but not yet supported" $
    runIO discover >>= mapM_ addTest

discover :: IO [FilePath]
discover = do
    present <- doesDirectoryExist fixtureDir
    if not present
        then pure []
        else sort . filter (".hs" `isSuffixOf`) <$> listDirectory fixtureDir

addTest :: FilePath -> Spec
addTest name = it (takeBaseName name) $ do
    msg <- readGapLine (fixtureDir </> name)
    pendingWith msg

-- Extract the first @-- Gap: ...@ line from the fixture so the hspec
-- pending message names a concrete gap (bucket + package + ref).
readGapLine :: FilePath -> IO String
readGapLine path = do
    contents <- readFile path
    let gapPrefix = "-- Gap: "
    pure $ case [drop (length gapPrefix) l | l <- lines contents, gapPrefix `isPrefixOf` l] of
        (msg:_) -> msg
        []      -> "unsupported feature; see " ++ path
