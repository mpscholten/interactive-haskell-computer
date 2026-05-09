-- | Hand-rolled CPP (the C preprocessor) — a tiny subset sufficient for
-- real-world Haskell sources from Hackage (bytestring, base-shim, etc.).
--
-- What we support:
--
--   * @#if@ / @#ifdef@ / @#ifndef@ / @#elif@ / @#else@ / @#endif@ with
--     nesting.
--   * @#define NAME value@ (object-like) and @#define NAME(a,b) value@
--     (function-like, the bare minimum for @MIN_VERSION_X(a,b,c)@).
--   * @#undef NAME@.
--   * @#include "path"@ — double-quoted form, resolved relative to the
--     directory of the including file.  The current macro table is
--     inherited by the included file (C semantics).  Include depth is
--     capped at 16 to prevent infinite recursion.
--   * Condition expressions with: integer literals, @defined(NAME)@,
--     @!@, @&&@, @||@, parens, comparison operators (@==@, @!=@, @<@,
--     @<=@, @>@, @>=@), and macro expansion (so
--     @MIN_VERSION_base(4,19,0)@ works).
--   * @#line@ and @<angle-bracket> #include@ forms are accepted and
--     skipped (no @<>@ lookup path).
--
-- What we deliberately don't support (Hackage doesn't lean on them):
-- token pasting @##@, stringification @#@, variadic macros.
--
-- Behaviour:
--   * The output has exactly the same number of newlines as the input
--     (directives and disabled blocks become blank lines), so line
--     numbers in error messages remain faithful to the original source.
--   * When a file does not contain any @#@ at column 1, the input is
--     returned unchanged (fast path).
--
-- Runtime shape: 'cppPreprocess' takes an initial 'CppContext' (pre-
-- defined macros) and a source 'ByteString', returns the transformed
-- bytes. 'defaultCppContext' is synthesized at startup from the host
-- GHC version.
module IHC.Cpp
    ( CppContext
    , MacroBody(..)
    , cppPreprocess
    , cppPreprocessWithIncludes
    , defaultCppContext
    , emptyCppContext
    , defineObject
    ) where

import Control.Exception (throwIO, Exception, catch, IOException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import Data.List (isSuffixOf)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, splitDrive, (</>))
import System.IO.Unsafe (unsafePerformIO)

-- | A macro body: either a simple replacement text, or a function-like
-- macro with parameter names and a replacement template containing
-- occurrences of those names.
data MacroBody
    = MObj !ByteString
    | MFun ![ByteString] !ByteString
    deriving (Eq, Show)

-- | The pre-defined macro table. 'cppPreprocess' threads an updated
-- copy through conditional evaluation but the input to the function is
-- a snapshot — the caller usually passes 'defaultCppContext'.
type CppContext = Map ByteString MacroBody

emptyCppContext :: CppContext
emptyCppContext = Map.empty

-- | Helper for constructing a context containing a handful of
-- object-like macros (test helper + phase-2.7 package glue).
defineObject :: ByteString -> ByteString -> CppContext -> CppContext
defineObject name body = Map.insert name (MObj body)

-- | Default predefined macros. We synthesize @__GLASGOW_HASKELL__=910@
-- (the only widely consulted host-GHC hook) and the always-true
-- @MIN_VERSION_base(ma,mi,pa)@ shim expected by most packages.
defaultCppContext :: CppContext
defaultCppContext = Map.fromList
    [ ("__GLASGOW_HASKELL__", MObj "910")
    , ("MIN_VERSION_base",    MFun ["ma","mi","pa"] "1")
    , ("MIN_VERSION_network", MFun ["ma","mi","pa"] "1")
    , ("MIN_VERSION_unix",    MFun ["ma","mi","pa"] "1")
    -- Architecture: we target macOS aarch64 (Apple Silicon).
    -- aarch64_HOST_ARCH is the GHC CPP convention for arm64 targets.
    -- __ARM_FEATURE_UNALIGNED reflects Apple Silicon's unaligned-access
    -- support (M1/M2/M3 all tolerate unaligned loads/stores natively).
    , ("aarch64_HOST_ARCH",        MObj "1")
    , ("__ARM_FEATURE_UNALIGNED",  MObj "1")
    -- Common bytestring guard macros — set here so they survive even if
    -- bytestring-cpp-macros.h redefines them as MIN_VERSION_base(...)
    -- expressions (the CEVar evaluator now handles that expansion, but
    -- having them pre-set to 1 also means #ifdef checks pass).
    , ("HS_cstringLength_AND_FinalPtr_AVAILABLE", MObj "1")
    , ("HS_unsafeWithForeignPtr_AVAILABLE",       MObj "1")
    , ("HS_timesInt2_PRIMOP_AVAILABLE",           MObj "1")
    ]

-- | Hard cap on include nesting depth to prevent infinite recursion.
maxIncludeDepth :: Int
maxIncludeDepth = 16

-- | GHC's global include directories, read once from the
-- @IHC_GHC_INCLUDE_DIRS@ environment variable (colon-separated).  These
-- contain headers like @MachDeps.h@, @HsFFI.h@, @HsBaseConfig.h@ that are
-- part of the installed GHC but are not shipped in Hackage source tarballs.
-- Populated lazily on first use via 'unsafePerformIO'; safe because the
-- environment variable is read-only after process start.
{-# NOINLINE ghcGlobalIncludeDirsRef #-}
ghcGlobalIncludeDirsRef :: IORef (Maybe [FilePath])
ghcGlobalIncludeDirsRef = unsafePerformIO (newIORef Nothing)

-- | Return the list of GHC global include directories, reading the env var
-- the first time this function is called.
getGhcGlobalIncludeDirs :: IO [FilePath]
getGhcGlobalIncludeDirs = do
    cached <- readIORef ghcGlobalIncludeDirsRef
    case cached of
        Just dirs -> pure dirs
        Nothing   -> do
            mEnv <- lookupEnv "IHC_GHC_INCLUDE_DIRS"
            let dirs = case mEnv of
                    Nothing  -> []
                    Just ""  -> []
                    Just val -> splitOnColon val
            atomicWriteIORef ghcGlobalIncludeDirsRef (Just dirs)
            pure dirs
  where
    splitOnColon :: String -> [FilePath]
    splitOnColon ""  = []
    splitOnColon str = case break (== ':') str of
        (d, [])     -> [d]
        (d, _:rest) -> d : splitOnColon rest

-- | Raised when @#include@ depth exceeds 'maxIncludeDepth'.
newtype IncludeDepthExceeded = IncludeDepthExceeded FilePath deriving Show
instance Exception IncludeDepthExceeded

-- | Raised when a @#include@ target file cannot be found.
data IncludeMissing = IncludeMissing
    { imIncluded :: FilePath   -- ^ the path as written in the directive
    , imResolved :: FilePath   -- ^ the absolute path we searched for
    } deriving Show
instance Exception IncludeMissing

--------------------------------------------------------------------------------
-- Preprocessor driver
--------------------------------------------------------------------------------

-- | Entry point. No-op if the source has no CPP directives.
-- Uses no extra include directories; see 'cppPreprocessWithIncludes' for
-- a variant that respects a package's @include-dirs:@ stanza.
cppPreprocess :: CppContext -> FilePath -> ByteString -> IO ByteString
cppPreprocess ctx0 path src = cppPreprocessWithIncludes [] ctx0 path src

-- | Like 'cppPreprocess' but also searches the given @includeDirs@ when
-- resolving a @#include "file"@ directive.  The search order is:
--   1. The directory of the file containing the @#include@.
--   2. Each directory in @includeDirs@ (in order).
--   3. The existing ancestor-@include\/@ heuristic as a last resort.
--
-- This is the entry point used by the scheduler when it has looked up
-- the owning package's @include-dirs:@ from its @.cabal@ file.
cppPreprocessWithIncludes :: [FilePath] -> CppContext -> FilePath -> ByteString -> IO ByteString
cppPreprocessWithIncludes includeDirs ctx0 path src
    | not (hasCppDirective src) = pure src
    | otherwise = do
        let ls = splitLines src
        (outLs, _) <- processIO includeDirs ctx0 True ls [] path 0
        pure (joinLines outLs)

-- | Quick scan: any `#` appearing at start of line? (Tolerates leading
-- whitespace in the scan.) If not, we skip the whole rewriting pass.
hasCppDirective :: ByteString -> Bool
hasCppDirective src = go 0 True
  where
    n = BS.length src
    go i atBol
        | i >= n    = False
        | otherwise = case BS.index src i of
            0x23 | atBol -> True                 -- '#' at BOL
            0x0A        -> go (i + 1) True
            c | c == 0x20 || c == 0x09 ->        -- keep atBol
                if atBol then go (i + 1) atBol else go (i + 1) atBol
              | otherwise -> go (i + 1) False

--------------------------------------------------------------------------------
-- Line-level processing
--
-- @enabled@ is whether the current @#if@ branch we're in is active.
-- We carry a stack of (wasAnyTaken, currentlyActive) for nested
-- conditionals; on @#elif@/@#else@ we toggle based on whether any
-- previous branch was taken.
--------------------------------------------------------------------------------

data BranchFrame = BranchFrame
    { bfAnyTaken :: !Bool        -- has any branch in this if-chain matched?
    , bfActive   :: !Bool        -- is the current branch active?
    , bfParent   :: !Bool        -- was the enclosing scope active?
    }

-- | Join backslash-continuation lines into a single logical line.
-- If @ln@ ends with a @\\@ (after stripping trailing horizontal whitespace),
-- consume lines from @rest@ recursively until a line without a trailing @\\@.
-- Returns @(logicalLine, continuationCount, remainingLines)@.
-- Each consumed continuation line will be replaced by a blank output line
-- by the caller, preserving source line numbers.
joinContinuations :: ByteString -> [ByteString] -> (ByteString, Int, [ByteString])
joinContinuations ln rest
    | endsWithBackslash ln =
        let ln' = stripTrailingBackslash ln
        in case rest of
            []     -> (ln', 0, [])
            (r:rs) -> let (joined, n, rest'') = joinContinuations r rs
                      in (ln' <> BC.singleton ' ' <> joined, n + 1, rest'')
    | otherwise = (ln, 0, rest)
  where
    endsWithBackslash l =
        let stripped = BC.dropWhileEnd isHSpace l
        in not (BS.null stripped) && BC.last stripped == '\\'
    stripTrailingBackslash l =
        -- Remove the trailing backslash, then strip any trailing whitespace
        -- that was before the backslash (and any after it, though there
        -- shouldn't be any after stripping).
        let stripped = BC.dropWhileEnd isHSpace l
        in BC.dropWhileEnd isHSpace (BC.init stripped)

-- | Drive line-by-line processing in IO (needed for #include).
--
-- Returns accumulated output lines (in order) and the final context.
-- For each input line, emit either the processed line (if active) or a
-- blank line (if skipped / directive) so line numbers match.
processIO
    :: [FilePath]        -- ^ extra include-dirs (from @include-dirs:@ in .cabal)
    -> CppContext
    -> Bool              -- ^ current scope is active
    -> [ByteString]      -- ^ remaining input lines
    -> [BranchFrame]     -- ^ branch stack
    -> FilePath          -- ^ path of the file being processed (for #include resolution)
    -> Int               -- ^ current include nesting depth
    -> IO ([ByteString], CppContext)
processIO _includeDirs ctx _active [] _ _ _ = pure ([], ctx)
processIO includeDirs ctx active (ln : rest) stack filePath depth =
    -- For directive lines (starting with optional whitespace then '#'),
    -- join any backslash-continuation lines into a single logical directive.
    -- Continuation lines are replaced with blank output lines to preserve
    -- line numbers. Regular (non-directive) lines are NOT joined.
    let isDirectiveLine l = case BC.dropWhile isHSpace l of
                                bs | not (BS.null bs) && BC.head bs == '#' -> True
                                _                                           -> False
        (logicalLn, contCount, rest')
            | isDirectiveLine ln = joinContinuations ln rest
            | otherwise          = (ln, 0, rest)
        contBlanks = replicate contCount BS.empty
        emit1 xs = BS.empty : contBlanks ++ xs
    in
    case parseDirective logicalLn of
        Just (DirIf expr) -> do
            let taken = active && evalCondExpr ctx expr
                frame = BranchFrame taken taken active
            (xs, c) <- processIO includeDirs ctx taken rest' (frame : stack) filePath depth
            pure (emit1 xs, c)

        Just (DirIfdef name) -> do
            let defined = Map.member name ctx
                taken   = active && defined
                frame   = BranchFrame taken taken active
            (xs, c) <- processIO includeDirs ctx taken rest' (frame : stack) filePath depth
            pure (emit1 xs, c)

        Just (DirIfndef name) -> do
            let defined = Map.member name ctx
                taken   = active && not defined
                frame   = BranchFrame taken taken active
            (xs, c) <- processIO includeDirs ctx taken rest' (frame : stack) filePath depth
            pure (emit1 xs, c)

        Just (DirElif expr) -> case stack of
            (f : fs) -> do
                let cond   = evalCondExpr ctx expr
                    newAct = bfParent f && not (bfAnyTaken f) && cond
                    f'     = f { bfAnyTaken = bfAnyTaken f || newAct
                               , bfActive   = newAct }
                (xs, c) <- processIO includeDirs ctx newAct rest' (f' : fs) filePath depth
                pure (emit1 xs, c)
            [] -> do  -- stray #elif — pretend blank
                (xs, c) <- processIO includeDirs ctx active rest' stack filePath depth
                pure (emit1 xs, c)

        Just DirElse -> case stack of
            (f : fs) -> do
                let newAct = bfParent f && not (bfAnyTaken f)
                    f'     = f { bfAnyTaken = bfAnyTaken f || newAct
                               , bfActive   = newAct }
                (xs, c) <- processIO includeDirs ctx newAct rest' (f' : fs) filePath depth
                pure (emit1 xs, c)
            [] -> do
                (xs, c) <- processIO includeDirs ctx active rest' stack filePath depth
                pure (emit1 xs, c)

        Just DirEndif -> case stack of
            (_ : fs) -> do
                let parent = case fs of
                        (f:_) -> bfActive f
                        []    -> True
                (xs, c) <- processIO includeDirs ctx parent rest' fs filePath depth
                pure (emit1 xs, c)
            [] -> do
                (xs, c) <- processIO includeDirs ctx active rest' stack filePath depth
                pure (emit1 xs, c)

        Just (DirDefine name body) -> do
            let ctx' | active    = Map.insert name body ctx
                     | otherwise = ctx
            (xs, c) <- processIO includeDirs ctx' active rest' stack filePath depth
            pure (emit1 xs, c)

        Just (DirUndef name) -> do
            let ctx' | active    = Map.delete name ctx
                     | otherwise = ctx
            (xs, c) <- processIO includeDirs ctx' active rest' stack filePath depth
            pure (emit1 xs, c)

        Just (DirInclude incPath) | active -> do
            -- Resolve relative to the directory of the including file.
            let dir      = takeDirectory filePath
                resolved = dir </> incPath
            -- Depth check.
            if depth >= maxIncludeDepth
                then throwIO (IncludeDepthExceeded resolved)
                else do
                    maybeBytes <- tryReadFile resolved
                    -- Fallback 1: search cabal include-dirs (from include-dirs: stanza).
                    -- This is the primary fix for packages like bytestring that ship
                    -- headers in a separate include/ directory.
                    maybeBytes1 <- case maybeBytes of
                        Just _  -> pure maybeBytes
                        Nothing -> searchIncludeDirs includeDirs incPath
                    -- Fallback 2: GHC's own installed include directories
                    -- (MachDeps.h, HsFFI.h, HsBaseConfig.h, DerivedConstants.h,
                    -- etc.).  Populated from IHC_GHC_INCLUDE_DIRS env var which
                    -- the nix shell sets to the ghcIncludeDirs derivation output.
                    maybeBytes2 <- case maybeBytes1 of
                        Just _  -> pure maybeBytes1
                        Nothing -> do
                            ghcDirs <- getGhcGlobalIncludeDirs
                            searchIncludeDirs ghcDirs incPath
                    -- Fallback 3: walk ancestor include/ directories heuristically.
                    -- Handles packages where the cabal include-dirs weren't parsed.
                    maybeBytes' <- case maybeBytes2 of
                        Just _  -> pure maybeBytes2
                        Nothing -> do
                            mAncestor <- findInAncestorIncludes dir incPath
                            case mAncestor of
                                Just ancestorPath -> tryReadFile ancestorPath
                                Nothing           -> pure Nothing
                    case maybeBytes' of
                        Just incBytes -> do
                            -- Read and preprocess the included file with the
                            -- current macro table (C semantics: #define from
                            -- the outer file is visible in the included file).
                            -- The resolved path for the included file should be
                            -- the path we actually found it at, so relative
                            -- includes inside it also resolve correctly.
                            ghcDirs <- getGhcGlobalIncludeDirs
                            resolvedPath <- findIncludePath dir (includeDirs ++ ghcDirs) incPath
                            let incLines = splitLines incBytes
                            (incOut, ctx') <- processIO includeDirs ctx True incLines [] resolvedPath (depth + 1)
                            -- The included lines replace the single #include
                            -- directive line; then continue with the rest of
                            -- the current file using the updated ctx'.
                            (xs, c) <- processIO includeDirs ctx' active rest' stack filePath depth
                            pure (incOut ++ contBlanks ++ xs, c)
                        Nothing
                            | isSystemHeader incPath -> do
                                -- Unresolvable system header (.h/.hpp/.hsc config
                                -- headers): silently drop the directive and emit
                                -- an empty replacement. Needed for generated
                                -- headers like @HsBaseConfig.h@ that only live
                                -- in the GHC build tree — this unblocks all of
                                -- @System.IO@, @GHC.IO.*@, @Foreign.C.*@.
                                -- A Haskell line-comment is emitted in place of
                                -- the directive for debug visibility (and to
                                -- preserve the source line number).
                                let debugComment = BC.pack ("-- IHC.Cpp: dropped unresolvable system include: " ++ incPath)
                                (xs, c) <- processIO includeDirs ctx active rest' stack filePath depth
                                pure (debugComment : contBlanks ++ xs, c)
                            | otherwise ->
                                throwIO (IncludeMissing incPath resolved)

        Just (DirInclude _) -> do
            -- #include in a disabled block: skip it.
            (xs, c) <- processIO includeDirs ctx active rest' stack filePath depth
            pure (emit1 xs, c)

        Just DirIgnore -> do
            (xs, c) <- processIO includeDirs ctx active rest' stack filePath depth
            pure (emit1 xs, c)

        Nothing -> do
            -- Regular line. Emit iff active; otherwise blank.
            -- Note: for regular lines, contCount=0 and rest'=rest, so no change.
            -- When active, expand object-like and function-like macros defined
            -- in the context so that e.g. IS_WINDOWS, CHAR, FILEPATH, MODULE_NAME
            -- (defined via #define in a template-include wrapper like Posix.hs)
            -- are substituted into the body of the included file.
            --
            -- Exception: C header files (.h, .hpp, .H) may contain C-style
            -- comments (/* ... */) and other non-Haskell content.  Emitting
            -- these verbatim into the output would confuse the Haskell lexer
            -- (e.g. bytestring-cpp-macros.h has /* ... */ comments between
            -- #define directives that appear before the module declaration
            -- of the including .hs file).  We blank those regular lines out
            -- so only CPP-directive-driven content affects the output.
            let isCHeader = ".h"   `isSuffixOf` filePath
                         || ".hpp" `isSuffixOf` filePath
                         || ".H"   `isSuffixOf` filePath
                (regularLn, regularContCount, regularRest)
                    | active && not isCHeader =
                        joinFunctionMacroInvocation ctx ln rest'
                    | otherwise = (ln, 0, rest')
            let emit | not active  = BS.empty
                     | isCHeader   = BS.empty   -- C comments / non-Haskell content
                     | otherwise   = expandMacrosInLine ctx regularLn
            (xs, c) <- processIO includeDirs ctx active regularRest stack filePath depth
            pure (emit : replicate regularContCount BS.empty ++ xs, c)

-- | Search a list of include directories for @incPath@, returning the bytes
-- of the first match or 'Nothing' if none is found.
searchIncludeDirs :: [FilePath] -> FilePath -> IO (Maybe ByteString)
searchIncludeDirs []       _       = pure Nothing
searchIncludeDirs (d : ds) incPath = do
    let candidate = d </> incPath
    mb <- tryReadFile candidate
    case mb of
        Just _  -> pure mb
        Nothing -> searchIncludeDirs ds incPath

-- | Return the absolute path at which @incPath@ was found, searching
-- first the source file's directory, then @includeDirs@, then the
-- ancestor-include heuristic.  Falls back to @dir </> incPath@ when
-- nothing matches (caller will either use the bytes already found or
-- handle the error separately).
findIncludePath :: FilePath -> [FilePath] -> FilePath -> IO FilePath
findIncludePath dir includeDirs incPath = do
    let direct = dir </> incPath
    directOk <- fileExists direct
    if directOk
        then pure direct
        else searchDirs includeDirs
  where
    searchDirs []       = do
        -- Try ancestor heuristic.
        mA <- findInAncestorIncludes dir incPath
        pure (maybe (dir </> incPath) id mA)
    searchDirs (d : ds) = do
        let candidate = d </> incPath
        ok <- fileExists candidate
        if ok then pure candidate else searchDirs ds

-- | True when an include path looks like a C/C++ system header rather than
-- a Haskell source file.  We identify these by their extension: @.h@, @.hpp@,
-- or any suffix that does not end in @.hs@ / @.lhs@.  Unresolvable system
-- headers are silently skipped rather than causing a hard error, because they
-- usually come from the GHC build tree (e.g. @HsBaseConfig.h@) which is not
-- present in the IHC source cache.
isSystemHeader :: FilePath -> Bool
isSystemHeader p =
       ".h"    `isSuffixOf` p
    || ".hpp"  `isSuffixOf` p
    || ".H"    `isSuffixOf` p
    || not (".hs"  `isSuffixOf` p || ".lhs" `isSuffixOf` p)

-- | Search ancestor directories for @include\/<incPath>@.
-- Many Hackage packages place their @.h@ headers in a top-level
-- @include\/@ directory rather than co-located with the @.hs@ source.
-- E.g. @bytestring@ ships @include\/bytestring-cpp-macros.h@, but
-- the source at @Data\/ByteString\/Internal\/Type.hs@ includes it as
-- @#include "bytestring-cpp-macros.h"@, expecting the compiler to
-- know the package root.  We replicate that by walking up the
-- directory tree until we hit the filesystem root or find a match.
findInAncestorIncludes :: FilePath -> FilePath -> IO (Maybe FilePath)
findInAncestorIncludes startDir incPath = go startDir
  where
    (drive, _) = splitDrive startDir
    go dir = do
        let candidate = dir </> "include" </> incPath
        exists <- fileExists candidate
        if exists
            then pure (Just candidate)
            else do
                let parent = takeDirectory dir
                if parent == dir || parent == drive
                    then pure Nothing
                    else go parent

-- | Try reading a file; return Nothing on any IO error.
tryReadFile :: FilePath -> IO (Maybe ByteString)
tryReadFile path = do
    exists <- fileExists path
    if exists
        then Just <$> BS.readFile path
        else pure Nothing

fileExists :: FilePath -> IO Bool
fileExists path = do
    result <- (BS.readFile path >> pure True) `catchIO` \_ -> pure False
    pure result

catchIO :: IO a -> (IOException -> IO a) -> IO a
catchIO = catch

--------------------------------------------------------------------------------
-- Directive recognition
--------------------------------------------------------------------------------

data Directive
    = DirIf      !CondExpr
    | DirIfdef   !ByteString
    | DirIfndef  !ByteString
    | DirElif    !CondExpr
    | DirElse
    | DirEndif
    | DirDefine  !ByteString !MacroBody
    | DirUndef   !ByteString
    | DirInclude !FilePath                 -- ^ double-quoted include path
    | DirIgnore                            -- #line, #pragma, #error, <> includes
    deriving Show

-- | If the line begins with @#@ (after optional whitespace), parse the
-- directive. Directives can span multiple physical lines via @\\@
-- continuations — we don't support that yet; real Hackage uses it rarely
-- inside Haskell sources.
parseDirective :: ByteString -> Maybe Directive
parseDirective ln =
    case BS.uncons (BC.dropWhile isHSpace ln) of
        Just (0x23, rest) ->
            let content = BC.dropWhile isHSpace rest
                (word, afterWord) = BC.span isIdentChar content
                arg = BC.dropWhile isHSpace afterWord
            in case word of
                "if"       -> Just (DirIf (parseCondExpr arg))
                "ifdef"    -> Just (DirIfdef (identOf arg))
                "ifndef"   -> Just (DirIfndef (identOf arg))
                "elif"     -> Just (DirElif (parseCondExpr arg))
                "else"     -> Just DirElse
                "endif"    -> Just DirEndif
                "define"   -> Just (parseDefine arg)
                "undef"    -> Just (DirUndef (identOf arg))
                "include"  -> Just (parseInclude arg)
                "line"     -> Just DirIgnore
                "pragma"   -> Just DirIgnore
                "error"    -> Just DirIgnore
                "warning"  -> Just DirIgnore
                ""         ->
                    -- Empty word: the '#' is not followed by an identifier.
                    -- If the next char is '-', this is '#-}' (Haskell
                    -- block-comment/pragma close) or '#-' — treat as a
                    -- regular line, NOT a CPP directive.  This preserves the
                    -- closing '#-}' of '{-# RULES ... #-}' pragmas that span
                    -- multiple lines when '#-}' appears at column 1.
                    case BC.uncons content of
                        Just ('-', _) -> Nothing   -- '#-}' or '#-...' → not CPP
                        _             -> Just DirIgnore  -- truly blank directive
                _          -> Just DirIgnore         -- unknown — be forgiving
        _ -> Nothing

-- | Parse the argument of @#include@. We support double-quoted
-- @\"path\"@ (relative include) and silently ignore angle-bracket
-- @\<path\>@ forms (system includes not needed for Haskell sources).
parseInclude :: ByteString -> Directive
parseInclude arg =
    case BC.uncons arg of
        Just ('"', rest) ->
            let (path, _) = BC.break (== '"') rest
            in DirInclude (BC.unpack path)
        _ -> DirIgnore   -- <angle> form or empty — skip

identOf :: ByteString -> ByteString
identOf bs =
    let (w, _) = BC.span isIdentChar bs
    in w

parseDefine :: ByteString -> Directive
parseDefine arg =
    let (name, afterName) = BC.span isIdentChar arg
    in case BC.uncons afterName of
        Just ('(', after) ->
            -- Function-like macro: #define NAME(a,b,c) body
            let (paramsRaw, afterParams) = BC.break (== ')') after
                params = map (BC.dropWhile isHSpace . BC.dropWhile isHSpace)
                             (splitComma paramsRaw)
                rest   = case BC.uncons afterParams of
                    Just (')', r) -> r
                    _             -> afterParams
                body   = BC.dropWhile isHSpace rest
            in DirDefine name (MFun (map stripWs params) (stripEnd body))
        _ ->
            let body = BC.dropWhile isHSpace afterName
            in DirDefine name (MObj (stripEnd body))

stripWs :: ByteString -> ByteString
stripWs = BC.dropWhile isHSpace . BC.reverse . BC.dropWhile isHSpace . BC.reverse

stripEnd :: ByteString -> ByteString
stripEnd = BC.reverse . BC.dropWhile isHSpace . BC.reverse

splitComma :: ByteString -> [ByteString]
splitComma bs
    | BS.null bs = []
    | otherwise  = case BC.break (== ',') bs of
        (a, rest) | BS.null rest -> [a]
                  | otherwise    -> a : splitComma (BS.drop 1 rest)

--------------------------------------------------------------------------------
-- Condition expressions
--
-- Grammar (approximation):
--
--   expr   ::= or
--   or     ::= and ('||' and)*
--   and    ::= cmp ('&&' cmp)*
--   cmp    ::= unary (('==' | '!=' | '<' | '<=' | '>' | '>=') unary)?
--   unary  ::= '!' unary | atom
--   atom   ::= number
--            | 'defined' '(' IDENT ')'
--            | 'defined' IDENT
--            | IDENT (macro expansion)
--            | '(' expr ')'
--            | IDENT '(' arg(,arg)* ')'  (function-like macro invocation)
--------------------------------------------------------------------------------

data CondExpr
    = CELit   !Integer
    | CENot   !CondExpr
    | CEAnd   !CondExpr !CondExpr
    | CEOr    !CondExpr !CondExpr
    | CECmp   !String   !CondExpr !CondExpr
    | CEDef   !ByteString
    | CEVar   !ByteString
    | CECall  !ByteString ![ByteString]    -- function-like macro call (args as raw text)
    deriving Show

-- | Trim trailing CR (from \r\n line endings) and comments (// /* ... */).
cleanCond :: ByteString -> ByteString
cleanCond = stripLineComment . stripCR

stripCR :: ByteString -> ByteString
stripCR bs = case BC.unsnoc bs of
    Just (r, '\r') -> r
    _              -> bs

stripLineComment :: ByteString -> ByteString
stripLineComment bs = case findSubstring "//" bs of
    Just i  -> BC.take i bs
    Nothing -> bs

-- | Locate the start index of the first occurrence of @needle@ in
-- @haystack@, or 'Nothing'. Small hand-rolled replacement so we don't
-- depend on the internal Char8 API.
findSubstring :: ByteString -> ByteString -> Maybe Int
findSubstring needle haystack
    | BS.null needle = Just 0
    | otherwise      = go 0
  where
    n = BS.length haystack
    m = BS.length needle
    go i
        | i + m > n = Nothing
        | BS.take m (BS.drop i haystack) == needle = Just i
        | otherwise = go (i + 1)

parseCondExpr :: ByteString -> CondExpr
parseCondExpr raw0 =
    let raw = cleanCond raw0
        (e, _) = pOr (BC.dropWhile isHSpace raw)
    in e

pOr :: ByteString -> (CondExpr, ByteString)
pOr bs =
    let (l, r1) = pAnd bs in loop l (BC.dropWhile isHSpace r1)
  where
    loop l r
        | "||" `BC.isPrefixOf` r =
            let r' = BC.dropWhile isHSpace (BS.drop 2 r)
                (rhs, r2) = pAnd r'
            in loop (CEOr l rhs) (BC.dropWhile isHSpace r2)
        | otherwise = (l, r)

pAnd :: ByteString -> (CondExpr, ByteString)
pAnd bs =
    let (l, r1) = pCmp bs in loop l (BC.dropWhile isHSpace r1)
  where
    loop l r
        | "&&" `BC.isPrefixOf` r =
            let r' = BC.dropWhile isHSpace (BS.drop 2 r)
                (rhs, r2) = pCmp r'
            in loop (CEAnd l rhs) (BC.dropWhile isHSpace r2)
        | otherwise = (l, r)

pCmp :: ByteString -> (CondExpr, ByteString)
pCmp bs =
    let (l, r) = pUnary bs
        r'     = BC.dropWhile isHSpace r
        try op opLen =
            let r'' = BC.dropWhile isHSpace (BS.drop opLen r')
                (rhs, r2) = pUnary r''
            in Just (CECmp op l rhs, r2)
    in case () of
        _ | "==" `BC.isPrefixOf` r' -> maybe (l, r) id (try "==" 2)
          | "!=" `BC.isPrefixOf` r' -> maybe (l, r) id (try "!=" 2)
          | "<=" `BC.isPrefixOf` r' -> maybe (l, r) id (try "<=" 2)
          | ">=" `BC.isPrefixOf` r' -> maybe (l, r) id (try ">=" 2)
          | "<"  `BC.isPrefixOf` r' -> maybe (l, r) id (try "<"  1)
          | ">"  `BC.isPrefixOf` r' -> maybe (l, r) id (try ">"  1)
          | otherwise                -> (l, r)

pUnary :: ByteString -> (CondExpr, ByteString)
pUnary bs = case BC.uncons (BC.dropWhile isHSpace bs) of
    Just ('!', rest) | not ("=" `BC.isPrefixOf` rest) ->
        let (e, r) = pUnary rest in (CENot e, r)
    _ -> pAtom bs

pAtom :: ByteString -> (CondExpr, ByteString)
pAtom bs0 =
    let bs = BC.dropWhile isHSpace bs0 in
    case BC.uncons bs of
        Just ('(', rest) ->
            let (e, r) = pOr (BC.dropWhile isHSpace rest)
                r'     = BC.dropWhile isHSpace r
            in case BC.uncons r' of
                Just (')', rr) -> (e, rr)
                _              -> (e, r')
        Just (c, _) | isDigit c ->
            let (numBs, rest) = BC.span isDigit bs
                n             = read (BC.unpack numBs) :: Integer
            in (CELit n, rest)
        Just (c, _) | isIdentStart c ->
            let (ident, rest) = BC.span isIdentChar bs
                rest'         = BC.dropWhile isHSpace rest
            in case ident of
                "defined" ->
                    case BC.uncons rest' of
                        Just ('(', r1) ->
                            let r1' = BC.dropWhile isHSpace r1
                                (nm, r2) = BC.span isIdentChar r1'
                                r2' = BC.dropWhile isHSpace r2
                            in case BC.uncons r2' of
                                Just (')', rr) -> (CEDef nm, rr)
                                _              -> (CEDef nm, r2')
                        _ ->
                            let (nm, r2) = BC.span isIdentChar rest'
                            in (CEDef nm, r2)
                _ -> case BC.uncons rest' of
                    Just ('(', r1) ->
                        let (argsRaw, after) = BC.break (== ')') r1
                            args = map stripWs (splitComma argsRaw)
                            r2   = case BC.uncons after of
                                Just (')', r) -> r
                                _             -> after
                        in (CECall ident args, r2)
                    _ -> (CEVar ident, rest)
        _ -> (CELit 0, bs)

evalCondExpr :: CppContext -> CondExpr -> Bool
evalCondExpr ctx = toBool . go
  where
    toBool 0 = False
    toBool _ = True

    go :: CondExpr -> Integer
    go (CELit n)    = n
    go (CENot e)    = if toBool (go e) then 0 else 1
    go (CEAnd a b)  = if toBool (go a) && toBool (go b) then 1 else 0
    go (CEOr  a b)  = if toBool (go a) || toBool (go b) then 1 else 0
    go (CECmp op a b) =
        let x = go a; y = go b in
        if (case op of
                "==" -> x == y
                "!=" -> x /= y
                "<"  -> x <  y
                "<=" -> x <= y
                ">"  -> x >  y
                ">=" -> x >= y
                _    -> False)
           then 1 else 0
    go (CEDef nm) = if Map.member nm ctx then 1 else 0
    go (CEVar nm) = case Map.lookup nm ctx of
        Just (MObj body) ->
            let body' = BC.strip body
            in case parseIntMaybe body' of
                Just n  -> n
                -- Body is not a bare integer: treat it as a CPP expression
                -- (e.g. HS_timesInt2_PRIMOP_AVAILABLE is defined as
                -- "MIN_VERSION_base(4,15,0)" by bytestring-cpp-macros.h).
                -- Re-parse and evaluate recursively so macro chains resolve.
                Nothing -> go (parseCondExpr body')
        Just (MFun _ _)  -> 0
        Nothing          -> 0
    go (CECall nm args) = case Map.lookup nm ctx of
        Just (MFun params body) ->
            let ctx' = foldr (\(p, a) c -> Map.insert p (MObj a) c)
                             ctx
                             (zip params args)
                expanded = parseCondExpr body
            in go (substExpr ctx' expanded)
        Just (MObj body) ->
            -- Object-like macro called with arguments: expand the body as a
            -- CPP expression (ignoring arguments, which is unusual but
            -- happens with macros like HS_UNALIGNED_ByteArray_OPS_OK that
            -- expand to a complex expression but take no args in practice).
            go (parseCondExpr (BC.strip body))
        Nothing       -> 0

    -- Substitute CEVar references against the inner context that has
    -- the function-like-macro's params bound. For MIN_VERSION_x this
    -- means "1" as the replacement.
    substExpr :: CppContext -> CondExpr -> CondExpr
    substExpr c = sub
      where
        sub (CEVar nm) = case Map.lookup nm c of
            Just (MObj body) ->
                let body' = BC.strip body
                in case parseIntMaybe body' of
                    Just n  -> CELit n
                    -- Non-integer body: treat as a nested expression.
                    Nothing -> parseCondExpr body'
            _ -> CEVar nm
        sub (CENot e)    = CENot (sub e)
        sub (CEAnd a b)  = CEAnd (sub a) (sub b)
        sub (CEOr  a b)  = CEOr  (sub a) (sub b)
        sub (CECmp o a b) = CECmp o (sub a) (sub b)
        sub other        = other

parseIntMaybe :: ByteString -> Maybe Integer
parseIntMaybe bs = case BC.unpack bs of
    s | all isDigit s && not (null s) -> Just (read s)
    _ -> Nothing

--------------------------------------------------------------------------------
-- Macro expansion in regular source lines
--------------------------------------------------------------------------------

-- | Expand object-like and function-like macros in a regular (non-directive)
-- source line.  This implements the core C-preprocessor substitution rule:
-- any identifier that names a macro in the context is replaced with its body,
-- and the result is rescanned (up to a recursion limit) so chains of macros
-- resolve correctly.
--
-- We skip expansion inside:
--   * @\"...\"@ double-quoted string literals
--   * @\'...\'@ character literals
--   * @--@ line comments (everything to end-of-line)
--   * @{- ... -}@ block comments (single-line occurrences only;
--     multi-line block comments carry their open state across lines, which
--     we approximate conservatively by not expanding after @{-@)
--
-- Recursion depth is capped at 16 to guard against self-referential macros.
expandMacrosInLine :: CppContext -> ByteString -> ByteString
expandMacrosInLine ctx ln
    | Map.null ctx = ln   -- fast path: nothing to expand
    | otherwise    = go ln 0
  where
    -- We rescan the result up to maxDepth times to resolve macro chains
    -- (e.g. @FILEPATH@ expands to @FilePath@ which doesn't need rescanning).
    maxDepth :: Int
    maxDepth = 16

    go :: ByteString -> Int -> ByteString
    go bs depth
        | depth >= maxDepth = bs
        | otherwise =
            let bs' = expandOnce bs
            in if bs' == bs then bs else go bs' (depth + 1)

    expandOnce :: ByteString -> ByteString
    expandOnce bs = BC.concat (scanLine bs False)
      where
        -- scanLine: process bytes, skipping over literals/comments.
        -- inBlockComment: True when we're past an unclosed {- on this line.
        scanLine :: ByteString -> Bool -> [ByteString]
        scanLine s _inBC
            | BS.null s = []
        scanLine s True =
            -- Inside a block comment: look for -} to close.
            case BC.uncons s of
                Just ('-', rest) | not (BS.null rest) && BC.head rest == '}' ->
                    let (prefix, afterClose) = BS.splitAt 2 s
                    in prefix : scanLine afterClose False
                Just (_, rest) -> BC.singleton (BC.head s) : scanLine rest True
                Nothing -> []
        scanLine s False =
            case BC.uncons s of
                Nothing -> []
                -- Line comment: rest of line is verbatim.
                Just ('-', rest) | not (BS.null rest) && BC.head rest == '-' ->
                    [s]
                -- Block comment open {-
                Just ('{', rest) | not (BS.null rest) && BC.head rest == '-' ->
                    BC.pack "{-" : scanLine (BS.drop 1 rest) True
                -- Double-quoted string literal: copy verbatim until closing ".
                Just ('"', rest) ->
                    let (lit, after) = scanStringLit rest
                    in BC.pack "\"" : lit : scanLine after False
                -- Char literal: copy verbatim until closing '.
                Just ('\'', rest) ->
                    let (lit, after) = scanCharLit rest
                    in BC.pack "'" : lit : scanLine after False
                -- Identifier start: check for macro.
                Just (c, _) | isIdentStart c ->
                    let (ident, afterIdent) = BC.span isIdentChar s
                    in case Map.lookup ident ctx of
                        -- Function-like macro: consume the argument list.
                        Just (MFun params body) ->
                            case consumeArgs afterIdent of
                                Just (args, afterArgs) ->
                                    let argMap = Map.fromList
                                            (zip params (map stripWs args))
                                        expanded = substituteParams argMap body
                                    in BC.pack (BC.unpack expanded)
                                       : scanLine afterArgs False
                                -- No '(' follows — treat as non-macro identifier.
                                Nothing -> ident : scanLine afterIdent False
                        -- Object-like macro: replace in-place.
                        Just (MObj body) ->
                            body : scanLine afterIdent False
                        -- Not a macro: copy verbatim.
                        Nothing -> ident : scanLine afterIdent False
                -- Any other character: copy verbatim.
                Just (c, rest) ->
                    BC.singleton c : scanLine rest False

-- | Join physical lines for a function-like macro invocation whose argument
-- list spans newlines, e.g. @STORABLE(Int,...,\n read,write)@.  CPP treats
-- newlines inside macro-call parentheses as whitespace; keeping this at the
-- process-line layer lets the later one-line expander stay simple while
-- preserving the input line count by emitting blanks for consumed lines.
joinFunctionMacroInvocation
    :: CppContext -> ByteString -> [ByteString]
    -> (ByteString, Int, [ByteString])
joinFunctionMacroInvocation ctx ln rest =
    case openFunctionMacroDepth ctx ln of
        Nothing -> (ln, 0, rest)
        Just d  -> gather d ln 0 rest
  where
    gather _ acc consumed [] = (acc, consumed, [])
    gather d acc consumed (r:rs) =
        let d'   = parenDepthContinue d r
            acc' = acc <> BC.singleton ' ' <> r
        in if d' <= 0
            then (acc', consumed + 1, rs)
            else gather d' acc' (consumed + 1) rs

openFunctionMacroDepth :: CppContext -> ByteString -> Maybe Int
openFunctionMacroDepth ctx = scan
  where
    scan s
        | BS.null s = Nothing
        | otherwise = case BC.uncons s of
            Just ('-', rest) | not (BS.null rest) && BC.head rest == '-' ->
                Nothing
            Just ('"', rest) ->
                let (_, after) = scanStringLit rest
                in scan after
            Just ('\'', rest) ->
                let (_, after) = scanCharLit rest
                in scan after
            Just (c, _) | isIdentStart c ->
                let (ident, afterIdent) = BC.span isIdentChar s
                    afterWs = BC.dropWhile isHSpace afterIdent
                in case Map.lookup ident ctx of
                    Just (MFun _ _) ->
                        case BC.uncons afterWs of
                            Just ('(', afterOpen) ->
                                case consumeBalancedParens 1 afterOpen of
                                    Left d      -> Just d
                                    Right after -> scan after
                            _ -> scan afterIdent
                    _ -> scan afterIdent
            Just (_, rest) -> scan rest
            Nothing -> Nothing

parenDepthContinue :: Int -> ByteString -> Int
parenDepthContinue depth0 = go depth0
  where
    go d s
        | d <= 0 || BS.null s = d
        | otherwise = case BC.uncons s of
            Just ('"', rest) ->
                let (_, after) = scanStringLit rest
                in go d after
            Just ('\'', rest) ->
                let (_, after) = scanCharLit rest
                in go d after
            Just ('(', rest) -> go (d + 1) rest
            Just (')', rest) -> go (d - 1) rest
            Just (_, rest)   -> go d rest
            Nothing          -> d

consumeBalancedParens :: Int -> ByteString -> Either Int ByteString
consumeBalancedParens depth0 = go depth0
  where
    go d s
        | d <= 0 = Right s
        | BS.null s = Left d
        | otherwise = case BC.uncons s of
            Just ('"', rest) ->
                let (_, after) = scanStringLit rest
                in go d after
            Just ('\'', rest) ->
                let (_, after) = scanCharLit rest
                in go d after
            Just ('(', rest) -> go (d + 1) rest
            Just (')', rest) ->
                let d' = d - 1
                in if d' == 0 then Right rest else go d' rest
            Just (_, rest)   -> go d rest
            Nothing          -> Left d

scanStringLit :: ByteString -> (ByteString, ByteString)
scanStringLit s = go s []
  where
    go bs acc = case BC.uncons bs of
        Nothing              -> (BC.pack (reverse acc), BS.empty)
        Just ('\\', rest) ->
            case BC.uncons rest of
                Nothing   -> (BC.pack (reverse acc), BS.empty)
                Just (e, rest') -> go rest' (e : '\\' : acc)
        Just ('"', rest)  -> (BC.pack (reverse ('"' : acc)), rest)
        Just (c, rest)    -> go rest (c : acc)

scanCharLit :: ByteString -> (ByteString, ByteString)
scanCharLit s = go s []
  where
    go bs acc = case BC.uncons bs of
        Nothing              -> (BC.pack (reverse acc), BS.empty)
        Just ('\\', rest) ->
            case BC.uncons rest of
                Nothing   -> (BC.pack (reverse acc), BS.empty)
                Just (e, rest') -> go rest' (e : '\\' : acc)
        Just ('\'', rest) -> (BC.pack (reverse ('\'' : acc)), rest)
        Just (c, rest)    -> go rest (c : acc)

-- Consume a function-like macro argument list @(arg1, arg2, ...)@ and
-- return (args, rest-after-close-paren).  Returns Nothing if the next
-- non-whitespace character is not @(@.
consumeArgs :: ByteString -> Maybe ([ByteString], ByteString)
consumeArgs s0 =
    let s = BC.dropWhile isHSpace s0
    in case BC.uncons s of
        Just ('(', rest) -> Just (collectArgs rest)
        _                -> Nothing

collectArgs :: ByteString -> ([ByteString], ByteString)
collectArgs = go 0 [] []
  where
    go _ cur groups bs | BS.null bs =
        (finish cur groups, BS.empty)
    go depth cur groups bs =
        case BC.uncons bs of
            Just ('(', rest) ->
                go (depth + 1) ('(' : cur) groups rest
            Just (')', rest)
                | depth == 0 -> (finish cur groups, rest)
                | otherwise  -> go (depth - 1) (')' : cur) groups rest
            Just (',', rest)
                | depth == 0 -> go depth [] (BC.pack (reverse cur) : groups) rest
                | otherwise  -> go depth (',' : cur) groups rest
            Just (c, rest) ->
                go depth (c : cur) groups rest
            Nothing -> (finish cur groups, BS.empty)

    finish cur groups =
        reverse (case (cur, groups) of
            ([], []) -> []
            _        -> BC.pack (reverse cur) : groups)

-- Substitute parameter names in a function-like macro body.
substituteParams :: Map ByteString ByteString -> ByteString -> ByteString
substituteParams paramMap body
    | Map.null paramMap = body
    | otherwise = BC.concat (go body)
  where
    go bs
        | BS.null bs = []
        | otherwise = case BC.uncons bs of
            Just (c, rest) | isIdentStart c ->
                let (ident, rest') = BC.span isIdentChar bs
                in case Map.lookup ident paramMap of
                    Just v  -> v : go rest'
                    Nothing -> ident : go rest'
            Just (c, rest) -> BC.singleton c : go rest
            Nothing -> []

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || isAlpha c

isIdentChar :: Char -> Bool
isIdentChar c = c == '_' || isAlphaNum c

-- | Split a ByteString on '\n'. Every newline produces an entry, and a
-- trailing newline produces an empty final element we can re-join with.
splitLines :: ByteString -> [ByteString]
splitLines bs
    | BS.null bs = []
    | otherwise  = case BC.break (== '\n') bs of
        (l, rest)
            | BS.null rest -> [l]
            | otherwise    -> l : splitLines (BS.drop 1 rest)

joinLines :: [ByteString] -> ByteString
joinLines = BS.intercalate "\n"
