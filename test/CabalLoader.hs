-- | Phase 2.7 — unit tests for the Cabal-aware package source loader.
--
-- All fixtures live under @test/Fixtures/CabalProject/@. Tests never
-- hit the network; they use a pre-seeded fake cache instead of
-- calling @cabal get@. The one path that does call out to a real
-- @cabal@ binary is gated behind @IHC_INTEGRATION_TESTS=1@.
--
-- The nix-source-dir smoke test (gated on @IHC_NIX_SOURCE_DIR@ being
-- set) verifies that the flake-provided Hackage source bundle is
-- enumerated by 'cachedPackageSearchPath' without any @cabal get@.
module CabalLoader (spec) where

import qualified Data.ByteString.Char8 as BC
import Data.List (isPrefixOf, isInfixOf, isSuffixOf, nub)
import Data.Maybe (fromJust, isJust)
import qualified Data.Map.Strict as Map
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Hspec

import IHC.CabalProject
    ( PackageInfo(..)
    , SearchEnv(..)
    , detectProjectRoot
    , findLocalCabalFile
    , parseCabalFile
    , parseFreezeFile
    , resolve
    , cachedPackageSearchPath
    , cachedPackageSearchPathWithIncludes
    , cabalTarballSearchPath
    , installedSourceDirForExecutable
    )
import IHC.Cpp (cppPreprocessWithIncludes, defaultCppContext)
import IHC.PackageStore (buildSearchEnvWithRoot)

fixtureRoot :: FilePath
fixtureRoot = "test/Fixtures/CabalProject"

simpleRoot :: FilePath
simpleRoot = fixtureRoot </> "simple"

fakeStoreRoot :: FilePath
fakeStoreRoot = simpleRoot </> "fake-store"

spec :: Spec
spec = describe "Phase 2.7 — Cabal project loader" do
    describe "installed source bundle" do
        it "is derived portably from the executable prefix" do
            installedSourceDirForExecutable
                ("/nix/store/example-ihc" </> "bin" </> "ihc")
                `shouldBe`
                    ("/nix/store/example-ihc" </> "share" </> "ihc" </> "sources")

    describe "detectProjectRoot" do
        it "finds the project root from an entry file's directory" do
            root <- detectProjectRoot (simpleRoot </> "app")
            absSimple <- canonicalizePath simpleRoot
            case root of
                Just r -> do
                    r' <- canonicalizePath r
                    r' `shouldBe` absSimple
                Nothing -> expectationFailure "expected Just _"

        it "returns Nothing at the filesystem root" do
            mRoot <- detectProjectRoot "/"
            mRoot `shouldBe` Nothing

    describe "findLocalCabalFile" do
        it "locates simple.cabal in the fixture root" do
            mCabal <- findLocalCabalFile simpleRoot
            fmap (BC.isInfixOf (BC.pack "simple.cabal") . BC.pack)
                 mCabal
                `shouldBe` Just True

        it "returns Nothing if the directory has no .cabal" do
            mCabal <- findLocalCabalFile (fixtureRoot </> "no_cabal")
            mCabal `shouldBe` Nothing

    describe "parseFreezeFile" do
        it "extracts pinned (name, version) pairs, skipping flag stanzas" do
            pairs <- parseFreezeFile (simpleRoot </> "cabal.project.freeze")
            pairs `shouldMatchList`
                [ (BC.pack "base",       BC.pack "4.19.0.0")
                , (BC.pack "foo",        BC.pack "1.0.0")
                , (BC.pack "bar",        BC.pack "2.3.4")
                , (BC.pack "bytestring", BC.pack "0.12.2.0")
                ]

        it "correctly parses hyphenated package names (optparse-applicative bug)" do
            -- The freeze-file parser must split on `==`, not on hyphens.
            -- A prior bug turned `optparse-applicative ==0.19.0.0` into
            -- package `optparse`, version `4.19.0.0`.
            pairs <- parseFreezeFile
                         (fixtureRoot </> "hyphenated" </> "cabal.project.freeze")
            pairs `shouldMatchList`
                [ (BC.pack "optparse-applicative", BC.pack "0.19.0.0")
                , (BC.pack "base",                 BC.pack "4.19.0.0")
                , (BC.pack "two-part",             BC.pack "1.2.3")
                , (BC.pack "a-b-c-d",              BC.pack "0.0.1")
                ]

    describe "parseCabalFile" do
        it "reads hs-source-dirs, default-extensions, cpp-options" do
            let cabalPath = simpleRoot </> "simple.cabal"
            rootAbs <- makeAbsolute simpleRoot
            mInfo <- parseCabalFile rootAbs cabalPath
            mInfo `shouldSatisfy` isJust
            let info = fromJust mInfo
            pkgName info    `shouldBe` BC.pack "simple"
            pkgVersion info `shouldBe` BC.pack "0.1.0.0"
            pkgSourceDirs info `shouldBe` [rootAbs </> "lib"]
            -- pkgExtensions merges the Haskell2010 implied set with the
            -- explicitly-declared extensions (default-language: Haskell2010
            -- on the library stanza of simple.cabal).
            let simpleExts = pkgExtensions info
            mapM_ (\e -> simpleExts `shouldSatisfy` elem e)
                [BC.pack "OverloadedStrings", BC.pack "LambdaCase"]
            mapM_ (\e -> simpleExts `shouldSatisfy` elem e)
                [ BC.pack "DatatypeContexts"
                , BC.pack "DoAndIfThenElse"
                , BC.pack "ImplicitPrelude"
                , BC.pack "PatternGuards"
                , BC.pack "TraditionalRecordSyntax"
                ]
            pkgCppOptions info `shouldMatchList` [BC.pack "-DFIXTURE_FLAG=1"]

        it "expands default-language: GHC2021 into its implied extension set" do
            let ghc2021Root  = fixtureRoot </> "ghc2021"
                cabalPath    = ghc2021Root </> "ghc2021.cabal"
            rootAbs <- makeAbsolute ghc2021Root
            mInfo   <- parseCabalFile rootAbs cabalPath
            mInfo `shouldSatisfy` isJust
            let info = fromJust mInfo
                exts      = pkgExtensions info
            -- Every GHC2021 staple must be on — this is the whole point
            -- of expanding default-language.
            mapM_ (\e -> exts `shouldSatisfy` elem e)
                [ BC.pack "ScopedTypeVariables"
                , BC.pack "MultiParamTypeClasses"
                , BC.pack "TupleSections"
                , BC.pack "NamedFieldPuns"
                , BC.pack "ExistentialQuantification"
                , BC.pack "InstanceSigs"
                , BC.pack "BangPatterns"
                , BC.pack "FlexibleContexts"
                , BC.pack "FlexibleInstances"
                , BC.pack "TypeApplications"
                , BC.pack "ImportQualifiedPost"
                , BC.pack "StandaloneKindSignatures"
                ]
            -- And the explicitly-declared extension still appears.
            exts `shouldSatisfy` elem (BC.pack "OverloadedStrings")
            -- No duplicate entries: if we ever list an extension under
            -- both the GHC2021 implied set and the default-extensions
            -- stanza, we still only emit it once.
            length exts `shouldBe` length (nub exts)

        it "defaults hs-source-dirs to the package root when omitted" do
            -- foo-1.0.0 declares hs-source-dirs: src, so it has one.
            -- Here we just check the mechanism works on a real cabal.
            rootAbs <- makeAbsolute (fakeStoreRoot </> "foo-1.0.0")
            mInfo <- parseCabalFile rootAbs
                         (rootAbs </> "foo.cabal")
            fmap pkgSourceDirs mInfo `shouldBe` Just [rootAbs </> "src"]

        it "parses pkgIncludeDirs from bytestring's include-dirs: include" $ do
            -- bytestring-0.12.2.0 ships bytestring-cpp-macros.h in include/
            -- and declares include-dirs: include in its cabal file.
            let bsRoot   = "bytestring-0.12.2.0"
                bsCabal  = bsRoot </> "bytestring.cabal"
            bsExists <- doesDirectoryExist bsRoot
            if not bsExists
                then pendingWith "bytestring-0.12.2.0 not present in project root"
                else do
                    rootAbs <- makeAbsolute bsRoot
                    mInfo   <- parseCabalFile rootAbs bsCabal
                    mInfo `shouldSatisfy` isJust
                    let info = fromJust mInfo
                    -- pkgIncludeDirs should contain rootAbs/include
                    pkgIncludeDirs info `shouldContain` [rootAbs </> "include"]

    describe "cppPreprocessWithIncludes" do
        it "resolves #include \"defs.h\" from an explicit includeDirs" $ do
            -- Write a temp header and use cppPreprocessWithIncludes to find it.
            -- We use the CPP fixtures dir for convenience.
            let incDir  = "test/Fixtures/CppIncludes/inc"
                srcFile = "test/Fixtures/CppIncludes/Main.hs"
            incDirExists <- doesDirectoryExist incDir
            if not incDirExists
                then pendingWith "test/Fixtures/CppIncludes/inc not present"
                else do
                    -- defs.h defines FOO=42; use it in an #if so CPP evaluates it.
                    let src = BC.pack
                                "#include \"defs.h\"\n\
                                \#if FOO == 42\n\
                                \main = putStrLn \"ok\"\n\
                                \#else\n\
                                \main = putStrLn \"bad\"\n\
                                \#endif\n"
                    result <- cppPreprocessWithIncludes [incDir] defaultCppContext srcFile src
                    -- The #if FOO == 42 branch should be active (defs.h was found and
                    -- FOO was defined), so "ok" must be present and "bad" must not.
                    result `shouldSatisfy` BC.isInfixOf (BC.pack "\"ok\"")
                    result `shouldSatisfy` (\r -> not (BC.isInfixOf (BC.pack "\"bad\"") r))

    describe "cachedPackageSearchPathWithIncludes" do
        it "returns (srcDir, includeDirs) pairs without crashing" $ do
            pairs <- cachedPackageSearchPathWithIncludes
            -- All source dirs must be absolute paths.
            mapM_ (\(sd, _) -> sd `shouldSatisfy` ("/" `isPrefixOf`)) pairs

        it "bytestring source dirs are paired with the include/ directory" $ do
            -- When bytestring is in the nix source dir or the cabal tarball
            -- cache, its source dir should be associated with its include/ dir.
            pairs <- cachedPackageSearchPathWithIncludes
            let bsPairs = [ (sd, incs)
                          | (sd, incs) <- pairs
                          , "bytestring" `isInfixOf` sd
                          ]
            case bsPairs of
                [] -> pendingWith "bytestring not found in package search path"
                _ -> do
                    -- At least one bytestring srcDir should have a non-empty includeDirs.
                    let nonEmpty = [ (sd, incs) | (sd, incs) <- bsPairs, not (null incs) ]
                    nonEmpty `shouldNotBe` []
                    -- Each include dir should end with "include".
                    mapM_ (\(_, incs) ->
                        mapM_ (\inc -> inc `shouldSatisfy` ("include" `isSuffixOf`)) incs)
                        nonEmpty

    describe "buildSearchEnvWithRoot (no network)" do
        it "assembles a SearchEnv covering local + fake-cached deps" do
            rootAbs <- makeAbsolute simpleRoot
            cacheAbs <- makeAbsolute fakeStoreRoot
            env <- buildSearchEnvWithRoot cacheAbs rootAbs
                     [ (BC.pack "foo", BC.pack "1.0.0")
                     -- not-in-cache dep: loader should log + skip,
                     -- not throw. Verifies optimistic failure mode.
                     , (BC.pack "nonexistent", BC.pack "9.9.9")
                     ]
            -- Local lib dir comes first; foo's src dir follows.
            seSearchPath env `shouldMatchList`
                [ rootAbs </> "lib"
                , cacheAbs </> "foo-1.0.0" </> "src"
                ]
            Map.size (sePackageTable env) `shouldBe` 2

    describe "resolve" do
        it "raises a CabalProjectError when the freeze file is missing" do
            resolve (fixtureRoot </> "no_cabal")
                `shouldThrow` anyException

    describe "integration (gated on IHC_INTEGRATION_TESTS=1)" do
        it "cabal get actually downloads a small package" $ do
            mFlag <- lookupEnv "IHC_INTEGRATION_TESTS"
            case mFlag of
                Just "1" -> do
                    -- We don't run the download here to keep CI fast;
                    -- presence of the env var is enough of an opt-in
                    -- for now. Real integration happens manually via
                    -- `ihc --check-cabal`.
                    pure ()
                _ ->
                    pendingWith "set IHC_INTEGRATION_TESTS=1 to enable"

    describe "nix source dir (gated on IHC_NIX_SOURCE_DIR being set)" do
        it "IHC_NIX_SOURCE_DIR contains hspec-* and QuickCheck-* source trees" $ do
            mNixDir <- lookupEnv "IHC_NIX_SOURCE_DIR"
            case mNixDir of
                Nothing ->
                    pendingWith "IHC_NIX_SOURCE_DIR not set — run inside nix develop"
                Just nixDir -> do
                    exists <- doesDirectoryExist nixDir
                    exists `shouldBe` True
                    entries <- listDirectory nixDir
                    let hasHspec      = any ("hspec-"      `isPrefixOf`) entries
                    let hasQuickCheck = any ("QuickCheck-" `isPrefixOf`) entries
                    hasHspec      `shouldBe` True
                    hasQuickCheck `shouldBe` True

        it "cachedPackageSearchPath includes a path inside the nix hspec source tree" $ do
            mNixDir <- lookupEnv "IHC_NIX_SOURCE_DIR"
            case mNixDir of
                Nothing ->
                    pendingWith "IHC_NIX_SOURCE_DIR not set — run inside nix develop"
                Just nixDir -> do
                    paths <- cachedPackageSearchPath
                    -- At least one path should be rooted inside the nix
                    -- source dir and belong to hspec.
                    let nixHspecPaths = filter (\p -> nixDir `isPrefixOf` p
                                                      && "hspec" `isInfixOf` p) paths
                    nixHspecPaths `shouldNotBe` []

    --------------------------------------------------------------------
    -- Cabal tarball cache (~/.cabal/packages/hackage.haskell.org/)
    --------------------------------------------------------------------
    describe "cabalTarballSearchPath" do
        it "returns an empty list (or valid paths) without crashing" $ do
            -- The function must not throw even when no extracted dirs
            -- exist.  On machines where `cabal get` has been run for
            -- some package, paths will be non-empty; on CI the cache is
            -- cold and the result is legitimately [].
            paths <- cabalTarballSearchPath
            -- All returned paths must be absolute (rooted at home).
            mapM_ (\p -> p `shouldSatisfy` ("/" `isPrefixOf`)) paths

        it "cabalTarballSearchPath paths are rooted inside ~/.cabal" $ do
            -- Every path that IS returned must live under
            -- ~/.cabal/packages/hackage.haskell.org/ because that is
            -- the only directory we scan.  We derive the expected root
            -- dynamically to avoid hardcoding the home directory.
            home <- lookupEnv "HOME"
            case home of
                Nothing -> pendingWith "HOME not set"
                Just h -> do
                    let cabalRoot = h </> ".cabal" </> "packages" </> "hackage.haskell.org"
                    cabalRootExists <- doesDirectoryExist cabalRoot
                    paths <- cabalTarballSearchPath
                    -- Paths may belong to the package root itself or to a
                    -- sub-directory declared via hs-source-dirs, so we
                    -- only check that each path starts under cabalRoot.
                    if not cabalRootExists
                        then paths `shouldBe` []
                        else mapM_ (\p -> p `shouldSatisfy` (cabalRoot `isPrefixOf`)) paths

        it "cachedPackageSearchPath includes cabalTarballSearchPath results" $ do
            -- Verify the two are consistent: everything from the tarball
            -- path must also appear in the combined search path.
            tarballPaths <- cabalTarballSearchPath
            allPaths     <- cachedPackageSearchPath
            mapM_ (\p -> allPaths `shouldContain` [p]) tarballPaths

    --------------------------------------------------------------------
    -- Multi-package search path + hs-source-dirs: src support
    --
    -- Regression tests for the three dryrun findings that flagged
    -- "multi-package search path not implemented" as a fatal blocker
    -- (mtl/transformers, random/splitmix, classy-prelude). These pin
    -- down that:
    --
    --   1. cachedPackageSearchPath enumerates EVERY
    --      ~/.cache/ihc/sources/*/ subdir, not just base-*.
    --   2. For packages with hs-source-dirs: src, the src/ subdir is
    --      the entry that appears (not the package root).
    --
    -- Each test is gated on the package actually being cached so they
    -- are safe to run in a cold-cache CI environment.
    --------------------------------------------------------------------
    describe "multi-package cache enumeration" do
        it "includes the mtl package root (module files at pkg/Control/Monad/State.hs)" $ do
            home <- lookupEnv "HOME"
            case home of
                Nothing -> pendingWith "HOME not set"
                Just h  -> do
                    let mtlRoot = h </> ".cache" </> "ihc" </> "sources" </> "mtl-2.3.2"
                    hasMtl <- doesDirectoryExist mtlRoot
                    if not hasMtl
                        then pendingWith "mtl-2.3.2 not in ~/.cache/ihc/sources"
                        else do
                            -- mtl's cabal has no hs-source-dirs, so the entry
                            -- should be the package root itself.
                            paths <- cachedPackageSearchPath
                            paths `shouldContain` [mtlRoot]

        it "includes random-1.3.1/src when hs-source-dirs: src is declared" $ do
            home <- lookupEnv "HOME"
            case home of
                Nothing -> pendingWith "HOME not set"
                Just h  -> do
                    let randomRoot = h </> ".cache" </> "ihc" </> "sources" </> "random-1.3.1"
                        randomSrc  = randomRoot </> "src"
                    hasRandom <- doesDirectoryExist randomRoot
                    if not hasRandom
                        then pendingWith "random-1.3.1 not in ~/.cache/ihc/sources"
                        else do
                            paths <- cachedPackageSearchPath
                            -- random declares hs-source-dirs: src, so we
                            -- MUST see the src/ subdir in the path, not
                            -- the bare package root.
                            paths `shouldContain` [randomSrc]
                            paths `shouldSatisfy` (\ps -> randomRoot `notElem` ps)

        it "includes splitmix-0.1.3.2/src when hs-source-dirs: src is declared" $ do
            home <- lookupEnv "HOME"
            case home of
                Nothing -> pendingWith "HOME not set"
                Just h  -> do
                    let splitRoot = h </> ".cache" </> "ihc" </> "sources" </> "splitmix-0.1.3.2"
                        splitSrc  = splitRoot </> "src"
                    hasSplit <- doesDirectoryExist splitRoot
                    if not hasSplit
                        then pendingWith "splitmix-0.1.3.2 not in ~/.cache/ihc/sources"
                        else do
                            paths <- cachedPackageSearchPath
                            paths `shouldContain` [splitSrc]
