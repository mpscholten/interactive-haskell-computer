-- | Lexer regression sweep over the full IHP source corpus.
--
-- For every @.hs@ file under the IHP checkout (default
-- @/Users/marc/digitallyinduced/ihp@, override via @IHC_IHP_ROOT@), drain
-- 'nextToken' from 'startCursor' to 'TkEof' and confirm:
--
--   1. no exception escapes — the lexer's only failure mode is the
--      @"unexpected byte"@ 'error' in "IHC.Lexer";
--   2. the final cursor reaches end-of-file, so a future regression that
--      emits 'TkEof' early is also caught.
--
-- Skipped with 'pendingWith' on machines without an IHP checkout.
module LexerIhp (spec) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf, sort)
import System.Directory
    ( doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink )
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeFileName)
import Test.Hspec

import IHC.Lexer (Cursor(..), Token(..), TokenKind(..), nextToken, startCursor)
import IHC.Source (Source, mkSource, srcBytes)

defaultIhpRoot :: FilePath
defaultIhpRoot = "/Users/marc/digitallyinduced/ihp"

spec :: Spec
spec = describe "Lexer — IHP source corpus" $ do
    mRoot <- runIO resolveRoot
    case mRoot of
        Nothing -> it "IHP root not present (skipped)" $
            pendingWith
                "set IHC_IHP_ROOT or place IHP at /Users/marc/digitallyinduced/ihp"
        Just root -> do
            files <- runIO (sort <$> walkHs root)
            mapM_ (addTest root) files

resolveRoot :: IO (Maybe FilePath)
resolveRoot = do
    env <- lookupEnv "IHC_IHP_ROOT"
    let root = maybe defaultIhpRoot id env
    present <- doesDirectoryExist root
    pure (if present then Just root else Nothing)

-- | Directory basenames we never descend into. Nix build outputs
-- (@result@, @result-doc@) and devenv state directories are symlinks or
-- copies of the entire nix store and would balloon the scan to gigabytes;
-- @.claude/@ holds duplicated worktrees of IHP itself.
skipDir :: FilePath -> Bool
skipDir name = name `elem`
    [ "dist-newstyle"
    , ".git"
    , ".claude"
    , "result"
    , "result-doc"
    , "node_modules"
    , ".devenv"
    , ".direnv"
    , "build"
    ]

walkHs :: FilePath -> IO [FilePath]
walkHs dir = do
    entries <- listDirectory dir
    concat <$> mapM (classify . (dir </>)) entries
  where
    classify p = do
        let name = takeFileName p
        if skipDir name
            then pure []
            else do
                -- Don't follow symlinks: IHP's @result/@ etc. point
                -- into the nix store, which is read-only and huge.
                isLink <- pathIsSymbolicLink p
                if isLink
                    then pure []
                    else do
                        isDir <- doesDirectoryExist p
                        if isDir
                            then walkHs p
                            else do
                                isFile <- doesFileExist p
                                pure [ p
                                     | isFile
                                     , ".hs" `isSuffixOf` p
                                     , name /= "Setup.hs"
                                     ]

addTest :: FilePath -> FilePath -> Spec
addTest root path =
    it (drop (length root + 1) path) $ do
        bytes <- BS.readFile path
        let src = mkSource path bytes
            end = BS.length (srcBytes src)
        result <- try (drainLex src startCursor)
                    :: IO (Either SomeException Cursor)
        case result of
            Left ex -> expectationFailure ("lexer threw: " <> show ex)
            Right finalCur ->
                cPos finalCur `shouldSatisfy` (>= end)

drainLex :: Source -> Cursor -> IO Cursor
drainLex s c = do
    let (Token k _ _ _ _, c') = nextToken s c
    case k of
        TkEof -> pure c'
        _     -> c' `seq` drainLex s c'
