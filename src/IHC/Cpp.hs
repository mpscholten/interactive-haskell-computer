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
import Data.List (isSuffixOf)
import System.FilePath (takeDirectory, splitDrive, (</>))
import System.IO (hPutStrLn, stderr)

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
    -- Common bytestring guard macros — accept whatever the current
    -- package configures; default to 1 (present) so the "happy path"
    -- branch is taken.
    , ("HS_cstringLength_AND_FinalPtr_AVAILABLE", MObj "1")
    , ("HS_unsafeWithForeignPtr_AVAILABLE",       MObj "1")
    , ("HS_timesInt2_PRIMOP_AVAILABLE",           MObj "1")
    ]

-- | Hard cap on include nesting depth to prevent infinite recursion.
maxIncludeDepth :: Int
maxIncludeDepth = 16

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
cppPreprocess :: CppContext -> FilePath -> ByteString -> IO ByteString
cppPreprocess ctx0 path src
    | not (hasCppDirective src) = pure src
    | otherwise = do
        let ls = splitLines src
        (outLs, _) <- processIO ctx0 True ls [] path 0
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

-- | Drive line-by-line processing in IO (needed for #include).
--
-- Returns accumulated output lines (in order) and the final context.
-- For each input line, emit either the processed line (if active) or a
-- blank line (if skipped / directive) so line numbers match.
processIO
    :: CppContext
    -> Bool              -- ^ current scope is active
    -> [ByteString]      -- ^ remaining input lines
    -> [BranchFrame]     -- ^ branch stack
    -> FilePath          -- ^ path of the file being processed (for #include resolution)
    -> Int               -- ^ current include nesting depth
    -> IO ([ByteString], CppContext)
processIO ctx _active [] _ _ _ = pure ([], ctx)
processIO ctx active (ln : rest) stack filePath depth =
    case parseDirective ln of
        Just (DirIf expr) -> do
            let taken = active && evalCondExpr ctx expr
                frame = BranchFrame taken taken active
            (xs, c) <- processIO ctx taken rest (frame : stack) filePath depth
            pure (BS.empty : xs, c)

        Just (DirIfdef name) -> do
            let defined = Map.member name ctx
                taken   = active && defined
                frame   = BranchFrame taken taken active
            (xs, c) <- processIO ctx taken rest (frame : stack) filePath depth
            pure (BS.empty : xs, c)

        Just (DirIfndef name) -> do
            let defined = Map.member name ctx
                taken   = active && not defined
                frame   = BranchFrame taken taken active
            (xs, c) <- processIO ctx taken rest (frame : stack) filePath depth
            pure (BS.empty : xs, c)

        Just (DirElif expr) -> case stack of
            (f : fs) -> do
                let cond   = evalCondExpr ctx expr
                    newAct = bfParent f && not (bfAnyTaken f) && cond
                    f'     = f { bfAnyTaken = bfAnyTaken f || newAct
                               , bfActive   = newAct }
                (xs, c) <- processIO ctx newAct rest (f' : fs) filePath depth
                pure (BS.empty : xs, c)
            [] -> do  -- stray #elif — pretend blank
                (xs, c) <- processIO ctx active rest stack filePath depth
                pure (BS.empty : xs, c)

        Just DirElse -> case stack of
            (f : fs) -> do
                let newAct = bfParent f && not (bfAnyTaken f)
                    f'     = f { bfAnyTaken = bfAnyTaken f || newAct
                               , bfActive   = newAct }
                (xs, c) <- processIO ctx newAct rest (f' : fs) filePath depth
                pure (BS.empty : xs, c)
            [] -> do
                (xs, c) <- processIO ctx active rest stack filePath depth
                pure (BS.empty : xs, c)

        Just DirEndif -> case stack of
            (_ : fs) -> do
                let parent = case fs of
                        (f:_) -> bfActive f
                        []    -> True
                (xs, c) <- processIO ctx parent rest fs filePath depth
                pure (BS.empty : xs, c)
            [] -> do
                (xs, c) <- processIO ctx active rest stack filePath depth
                pure (BS.empty : xs, c)

        Just (DirDefine name body) -> do
            let ctx' | active    = Map.insert name body ctx
                     | otherwise = ctx
            (xs, c) <- processIO ctx' active rest stack filePath depth
            pure (BS.empty : xs, c)

        Just (DirUndef name) -> do
            let ctx' | active    = Map.delete name ctx
                     | otherwise = ctx
            (xs, c) <- processIO ctx' active rest stack filePath depth
            pure (BS.empty : xs, c)

        Just (DirInclude incPath) | active -> do
            -- Resolve relative to the directory of the including file.
            let dir      = takeDirectory filePath
                resolved = dir </> incPath
            -- Depth check.
            if depth >= maxIncludeDepth
                then throwIO (IncludeDepthExceeded resolved)
                else do
                    maybeBytes <- tryReadFile resolved
                    -- Fallback: search ancestor include/ directories.
                    -- This handles Hackage packages that ship headers in
                    -- a top-level include/ dir (e.g. bytestring).
                    maybeBytes' <- case maybeBytes of
                        Just _  -> pure maybeBytes
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
                            let incLines = splitLines incBytes
                            (incOut, ctx') <- processIO ctx True incLines [] resolved (depth + 1)
                            -- The included lines replace the single #include
                            -- directive line; then continue with the rest of
                            -- the current file using the updated ctx'.
                            (xs, c) <- processIO ctx' active rest stack filePath depth
                            pure (incOut ++ xs, c)
                        Nothing
                            | isSystemHeader incPath -> do
                                -- Unresolvable system header (.h/.hpp/.hsc config
                                -- headers): emit a warning and produce zero lines.
                                -- This is safe for generated headers like
                                -- HsBaseConfig.h that live only in the build tree.
                                hPutStrLn stderr ("IHC.Cpp: skipping unresolvable system include: " ++ incPath)
                                (xs, c) <- processIO ctx active rest stack filePath depth
                                pure (BS.empty : xs, c)
                            | otherwise ->
                                throwIO (IncludeMissing incPath resolved)

        Just (DirInclude _) -> do
            -- #include in a disabled block: skip it.
            (xs, c) <- processIO ctx active rest stack filePath depth
            pure (BS.empty : xs, c)

        Just DirIgnore -> do
            (xs, c) <- processIO ctx active rest stack filePath depth
            pure (BS.empty : xs, c)

        Nothing -> do
            -- Regular line. Emit iff active; otherwise blank.
            let emit = if active then ln else BS.empty
            (xs, c) <- processIO ctx active rest stack filePath depth
            pure (emit : xs, c)

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

-- | Read a file, throwing 'IncludeMissing' if it doesn't exist.
readFileOrThrow :: FilePath -> FilePath -> IO ByteString
readFileOrThrow resolved original = do
    result <- tryReadFile resolved
    case result of
        Just bs -> pure bs
        Nothing -> throwIO (IncludeMissing original resolved)

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
                ""         -> Just DirIgnore         -- blank directive
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
        Just (MObj body) -> case parseIntMaybe (BC.strip body) of
            Just n  -> n
            Nothing -> 0          -- undefined numeric → 0 per CPP rules
        Just (MFun _ _)  -> 0
        Nothing          -> 0
    go (CECall nm args) = case Map.lookup nm ctx of
        Just (MFun params body) ->
            let ctx' = foldr (\(p, a) c -> Map.insert p (MObj a) c)
                             ctx
                             (zip params args)
                expanded = parseCondExpr body
            in go (substExpr ctx' expanded)
        Just (MObj _) -> 0        -- called an object-like macro; unusual
        Nothing       -> 0

    -- Substitute CEVar references against the inner context that has
    -- the function-like-macro's params bound. For MIN_VERSION_x this
    -- means "1" as the replacement.
    substExpr :: CppContext -> CondExpr -> CondExpr
    substExpr c = sub
      where
        sub (CEVar nm) = case Map.lookup nm c of
            Just (MObj body) -> case parseIntMaybe (BC.strip body) of
                Just n  -> CELit n
                Nothing -> CELit 0
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
