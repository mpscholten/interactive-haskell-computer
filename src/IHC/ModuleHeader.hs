-- | Module-header / import parsing.
--
-- Phase 2.5: read the optional @module Name [(exports)] where@ line and
-- every @import@ that follows at column 1, stopping at the first
-- non-import column-1 token. The parser works directly on the lexer
-- token stream — it produces no AST for expressions, only the
-- module-level skeleton.
--
-- Supported import forms:
--
--   import Foo
--   import Foo.Bar.Baz
--   import qualified Foo as B
--   import Foo (a, b, Tree(..))
--   import Foo hiding (c, d)
--
-- Not yet supported (designed-out of 2.5): operator imports
-- @import Foo ((<>))@, package-qualified imports
-- @import \"base\" Data.List@, and @module Foo (module Bar) where@
-- re-exports. These degrade gracefully — the scanner stops at the
-- unrecognised form and the rest of the file is still reachable.
module IHC.ModuleHeader
    ( ModuleName
    , ModuleHeader(..)
    , ImportDecl(..)
    , ImportSpec(..)
    , ExportSpec(..)
    , ExportItem(..)
    , parseModuleHeader
    , parseSingleImport
    , modulePathCandidates
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Word (Word8)
import System.FilePath ((</>), (<.>))

import IHC.Lexer
import IHC.Source

-- | A fully-qualified, dot-separated module name (e.g. @\"Data.ByteString.Lazy\"@).
type ModuleName = ByteString

data ModuleHeader = ModuleHeader
    { mhName    :: !(Maybe ModuleName)
    , mhExports :: !ExportSpec
    , mhImports :: ![ImportDecl]
    } deriving (Eq, Show)

data ImportDecl = ImportDecl
    { impModule    :: !ModuleName
    , impQualified :: !Bool
    , impAlias     :: !(Maybe ModuleName)
    , impSpec      :: !ImportSpec
    } deriving (Eq, Show)

data ImportSpec
    = ImportAll                     -- ^ no list: everything exported
    | ImportOnly   ![ByteString]    -- ^ @import Foo (a, b)@
    | ImportHiding ![ByteString]    -- ^ @import Foo hiding (c, d)@
    deriving (Eq, Show)

data ExportSpec
    = ExportAll                     -- ^ no list
    | ExportList ![ExportItem]
    deriving (Eq, Show)

data ExportItem
    = ExportName !ByteString
    | ExportType !ByteString !(Maybe [ByteString])
      -- ^ @Tree@ (Nothing), @Tree(..)@ (Just []), @Tree(Leaf,Node)@ (Just [...])
    | ExportModule !ByteString
      -- ^ @module Foo.Bar@ re-export form
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Entry point
--------------------------------------------------------------------------------

-- | Parse an optional module header + every leading import. Returns the
-- header (or 'Nothing' if this file has no module line and no imports)
-- together with the cursor positioned right before the first
-- non-header/import content. Never throws on malformed input — the
-- scanner gives up gracefully so the rest of the pipeline can still
-- read bindings.
parseModuleHeader :: Source -> Cursor -> IO (Maybe ModuleHeader, Cursor)
parseModuleHeader src cur0 = do
    let (tok, _) = skipNewlines src cur0
    case tkKind tok of
        TkModule -> do
            -- Advance past the `module` keyword.
            let (_, curAfterMod) = nextSigTok src cur0
            (name, exports, cur2) <- parseModuleLine src curAfterMod
            (imports, cur3) <- parseImports src cur2
            pure (Just (ModuleHeader (Just name) exports imports), cur3)
        TkImport -> do
            (imports, cur2) <- parseImports src cur0
            pure ( Just (ModuleHeader Nothing ExportAll imports)
                 , cur2)
        _ -> pure (Nothing, cur0)

-- | Produce the candidate relative-file paths for a dotted module name.
-- For now we try @Name.hs@ with dots replaced by @/@. The caller joins
-- each candidate with each entry in the search path.
modulePathCandidates :: ModuleName -> [FilePath]
modulePathCandidates name =
    let parts = BC.split '.' name
        slash = BC.intercalate (BC.pack "/") parts
    in [BC.unpack slash <.> "hs"]

--------------------------------------------------------------------------------
-- module-line parser
--------------------------------------------------------------------------------

parseModuleLine :: Source -> Cursor -> IO (ModuleName, ExportSpec, Cursor)
parseModuleLine src cur0 = do
    -- After `module`, expect dotted name.
    (mName, cur1) <- parseDottedName src cur0
    let name = case mName of
            Just n  -> n
            Nothing -> "Main"
    -- Optional export list, then `where`.
    let (tok, curT) = nextSigTok src cur1
    case tkKind tok of
        TkLParen -> do
            (exports, cur2) <- parseExportList src curT
            let (wTok, curW) = nextSigTok src cur2
            case tkKind wTok of
                TkWhere -> pure (name, exports, curW)
                _       -> pure (name, exports, cur2)
        TkWhere -> pure (name, ExportAll, curT)
        _ -> pure (name, ExportAll, cur1)

-- | Parse a dotted qualified name (e.g. @Data.ByteString.Lazy@). The
-- first token must be a 'TkConId'. Subsequent segments are joined only
-- if the dot abuts (no whitespace between @ConId@, @.@, and next
-- @ConId@) — this prevents greedy consumption of a real @.@ operator.
parseDottedName :: Source -> Cursor -> IO (Maybe ModuleName, Cursor)
parseDottedName src cur0 = do
    let (tok, cur1) = nextSigTok src cur0
    case tkKind tok of
        TkConId first -> go first tok cur1
        _ -> pure (Nothing, cur0)
  where
    go acc lastTok cur = do
        -- Peek without skipping newlines / whitespace: the dot must
        -- touch the previous ConId.
        let (dotTok, curAfterDot) = nextToken src cur
        case tkKind dotTok of
            TkDot | tkStart dotTok == tkEnd lastTok -> do
                let (conTok, curAfterCon) = nextToken src curAfterDot
                case tkKind conTok of
                    TkConId seg | tkStart conTok == tkEnd dotTok -> do
                        let acc' = acc <> BC.pack "." <> seg
                        go acc' conTok curAfterCon
                    _ -> pure (Just acc, cur)
            _ -> pure (Just acc, cur)

--------------------------------------------------------------------------------
-- export list
--------------------------------------------------------------------------------

-- | Called just after the opening @(@ of an export list. Consumes through
-- the closing @)@. On a malformed list we abort at the first surprise
-- and return what we have.
parseExportList :: Source -> Cursor -> IO (ExportSpec, Cursor)
parseExportList src cur0 = go [] cur0
  where
    go acc cur = do
        let (tok, cur1) = nextSigTok src cur
        case tkKind tok of
            TkRParen -> pure (ExportList (reverse acc), cur1)
            TkComma  -> go acc cur1
            TkIdent n   -> go (ExportName n : acc) cur1
            -- `module Foo.Bar` re-export form.
            TkModule -> do
                (mMod, cur2) <- parseDottedName src cur1
                case mMod of
                    Just modN -> go (ExportModule modN : acc) cur2
                    Nothing   -> pure (ExportList (reverse acc), cur1)
            TkConId n -> do
                -- Check for Tree(..) or Tree(a,b).
                let (peek, curP) = nextSigTok src cur1
                case tkKind peek of
                    TkLParen -> do
                        (subs, cur2) <- parseExportSubs src curP
                        go (ExportType n (Just subs) : acc) cur2
                    _ -> go (ExportType n Nothing : acc) cur1
            TkEof    -> pure (ExportList (reverse acc), cur1)
            -- Operator export: `(++)`, `(<>)`, etc. — skip the whole
            -- `(op)` group and continue rather than aborting the list.
            TkLParen -> do
                curSkip <- skipToCloseParen src cur1 1
                go acc curSkip
            _        -> pure (ExportList (reverse acc), cur)

    parseExportSubs s c0 = loop [] c0
      where
        loop subs c = do
            let (tok, c1) = nextSigTok s c
            case tkKind tok of
                TkRParen  -> pure (reverse subs, c1)
                TkDotDot  -> do
                    let (close, c2) = nextSigTok s c1
                    case tkKind close of
                        TkRParen -> pure ([], c2)      -- Tree(..)
                        _        -> pure (reverse subs, c1)
                TkComma   -> loop subs c1
                TkIdent n -> loop (n : subs) c1
                TkConId n -> loop (n : subs) c1
                _         -> pure (reverse subs, c)

--------------------------------------------------------------------------------
-- import block
--------------------------------------------------------------------------------

-- | Consume every @import@ declaration at column 1 starting from @cur0@,
-- stopping at the first non-import column-1 token (or EOF).
parseImports :: Source -> Cursor -> IO ([ImportDecl], Cursor)
parseImports src cur0 = go [] cur0
  where
    go acc cur = do
        let (tok, _) = skipNewlines src cur
        case tkKind tok of
            TkImport | tkCol tok == 1 -> do
                -- Re-scan to actually advance past TkImport.
                let (_, curAfterImp) = skipNewlines src cur
                let (_, curBody) = nextToken src curAfterImp
                mDecl <- parseOneImport src curBody
                case mDecl of
                    Just (decl, curNext) -> go (decl : acc) curNext
                    Nothing -> pure (reverse acc, cur)
            _ -> pure (reverse acc, cur)

-- | Starting just after the @import@ keyword, read a single declaration.
parseOneImport :: Source -> Cursor -> IO (Maybe (ImportDecl, Cursor))
parseOneImport src cur0 = do
    let (t1, cur1) = nextSigTok src cur0
    (qualified, curQ) <- case tkKind t1 of
        TkQualified -> pure (True, cur1)
        _           -> pure (False, cur0)
    (mModName, cur2) <- parseDottedName src curQ
    case mModName of
        Nothing   -> pure Nothing
        Just modN -> do
            -- Optional: as Alias
            let (t2, cur3) = nextSigTok src cur2
            (alias, cur4) <- case tkKind t2 of
                TkAs -> do
                    (mAlias, curA) <- parseDottedName src cur3
                    pure (mAlias, curA)
                _ -> pure (Nothing, cur2)
            -- Optional: hiding (...) or (...)
            let (t3, cur5) = nextSigTok src cur4
            (spec, curF) <- case tkKind t3 of
                TkHiding -> do
                    let (lp, curLp) = nextSigTok src cur5
                    case tkKind lp of
                        TkLParen -> do
                            (names, curEnd) <- parseImportList src curLp
                            pure (ImportHiding names, curEnd)
                        _ -> pure (ImportAll, cur4)
                TkLParen -> do
                    (names, curEnd) <- parseImportList src cur5
                    pure (ImportOnly names, curEnd)
                _ -> pure (ImportAll, cur4)
            let decl = ImportDecl
                    { impModule    = modN
                    , impQualified = qualified
                    , impAlias     = alias
                    , impSpec      = spec
                    }
            pure (Just (decl, curF))

-- | Called just after the opening @(@. Returns the list of imported
-- names and the cursor past the closing @)@.
parseImportList :: Source -> Cursor -> IO ([ByteString], Cursor)
parseImportList src cur0 = go [] cur0
  where
    go acc cur = do
        let (tok, cur1) = nextSigTok src cur
        case tkKind tok of
            TkRParen -> pure (reverse acc, cur1)
            TkComma  -> go acc cur1
            TkIdent n -> nameOrSub acc n cur1
            TkConId n -> nameOrSub acc n cur1
            TkEof    -> pure (reverse acc, cur1)
            _        -> pure (reverse acc, cur)

    -- After a name, check for a `(..)` or `(sub, sub)` tail, and skip it.
    nameOrSub acc n cur = do
        let (peek, curP) = nextSigTok src cur
        case tkKind peek of
            TkLParen -> do
                curClose <- skipToCloseParen src curP 1
                go (n : acc) curClose
            _ -> go (n : acc) cur

-- | Fast-forward through a balanced paren group. @depth@ is already 1.
skipToCloseParen :: Source -> Cursor -> Int -> IO Cursor
skipToCloseParen src cur0 = go cur0
  where
    go cur depth
        | depth <= 0 = pure cur
        | otherwise = do
            let (tok, cur') = nextSigTok src cur
            case tkKind tok of
                TkLParen -> go cur' (depth + 1)
                TkRParen -> go cur' (depth - 1)
                TkEof    -> pure cur'
                _        -> go cur' depth

--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

-- | Next significant token, skipping newlines.
nextSigTok :: Source -> Cursor -> (Token, Cursor)
nextSigTok src cur =
    let (t, c) = nextToken src cur in
    case tkKind t of
        TkNewline -> nextSigTok src c
        _         -> (t, c)

-- | Same as 'nextSigTok' but doesn't actually consume the token — the
-- returned cursor is still before the token, and the token is just
-- peeked. Used by 'parseImports' to re-check the column-1 property
-- without moving forward.
skipNewlines :: Source -> Cursor -> (Token, Cursor)
skipNewlines src cur =
    let (t, _) = nextToken src cur in
    case tkKind t of
        TkNewline ->
            let (_, c') = nextToken src cur in
            skipNewlines src c'
        _ -> (t, cur)

--------------------------------------------------------------------------------
-- Single-import entry point (used by the REPL)
--------------------------------------------------------------------------------

-- | Parse a single @import@ declaration from a 'Source' that contains
-- exactly one import line (e.g. @\"import qualified Foo as F\"@). The
-- leading @import@ keyword may be present or absent — the parser
-- handles both forms so the caller doesn't have to strip it.
--
-- Returns 'Nothing' if no valid 'ImportDecl' can be parsed from the
-- input (malformed or empty). Never throws.
parseSingleImport :: Source -> IO (Maybe ImportDecl)
parseSingleImport src = do
    let cur0 = startCursor
        (tok, _) = skipNewlines src cur0
    -- Skip an optional leading `import` keyword.
    let curBody = case tkKind tok of
            TkImport ->
                let (_, afterImp) = nextSigTok src cur0
                in afterImp
            _ -> cur0
    mResult <- parseOneImport src curBody
    pure (fmap fst mResult)
