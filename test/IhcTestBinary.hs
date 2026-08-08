module IhcTestBinary (ihcBin) where

import Control.Exception (IOException, catch)
import Data.List (isPrefixOf)
import System.Directory (doesFileExist, findExecutable, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))

ihcBin :: IO FilePath
ihcBin = do
    mEnv <- lookupEnv "IHC_BIN"
    case mEnv of
        Just path -> requireExecutable "IHC_BIN" path
        Nothing -> do
            mBuilt <- findCabalBuiltIhc
            case mBuilt of
                Just path -> pure path
                Nothing -> do
                    mPath <- findExecutable "ihc"
                    case mPath of
                        Just path -> pure path
                        Nothing -> ioError (userError missingMessage)

requireExecutable :: String -> FilePath -> IO FilePath
requireExecutable label path = do
    exists <- doesFileExist path
    if exists
        then pure path
        else ioError (userError (label <> " points at a missing ihc executable: " <> path))

findCabalBuiltIhc :: IO (Maybe FilePath)
findCabalBuiltIhc = firstExisting =<< cabalBuildCandidates

cabalBuildCandidates :: IO [FilePath]
cabalBuildCandidates = do
    let root = "dist-newstyle" </> "build"
    platforms <- listDirectorySafe root
    concat <$> traverse (platformCandidates root) platforms

platformCandidates :: FilePath -> FilePath -> IO [FilePath]
platformCandidates root platform = do
    let platformDir = root </> platform
    compilers <- listDirectorySafe platformDir
    concat <$> traverse (compilerCandidates platformDir) compilers

compilerCandidates :: FilePath -> FilePath -> IO [FilePath]
compilerCandidates platformDir compiler = do
    let compilerDir = platformDir </> compiler
    packages <- listDirectorySafe compilerDir
    pure
        [ compilerDir </> pkg </> "x" </> "ihc" </> "build" </> "ihc" </> "ihc"
        | pkg <- packages
        , "ihc-" `isPrefixOf` pkg
        ]

listDirectorySafe :: FilePath -> IO [FilePath]
listDirectorySafe path = listDirectory path `catch` handle
  where
    handle :: IOException -> IO [FilePath]
    handle _ = pure []

firstExisting :: [FilePath] -> IO (Maybe FilePath)
firstExisting [] = pure Nothing
firstExisting (path : paths) = do
    exists <- doesFileExist path
    if exists
        then pure (Just path)
        else firstExisting paths

missingMessage :: String
missingMessage =
    "Could not locate the ihc executable. Set IHC_BIN or run `cabal build exe:ihc`."
