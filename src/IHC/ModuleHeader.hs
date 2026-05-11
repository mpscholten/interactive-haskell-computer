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
--   import Foo ((.&.), (++))    -- operator imports are skipped (name ignored)
--
-- Package-qualified imports (@import \"base\" Data.List@ and
-- @import qualified \"base\" Data.List as L@, from the GHC
-- @PackageImports@ extension) are accepted: the package-name string
-- between @qualified@ (if present) and the module name is parsed and
-- silently discarded, since ihc resolves modules by name across its
-- source cache.
--
-- Not yet supported: @module Foo (module Bar) where@ re-exports. These
-- degrade gracefully — the scanner stops at the unrecognised form and
-- the rest of the file is still reachable.
-- Operator imports like @import Foo ((<>))@ are now parsed correctly
-- (the operator name is skipped/ignored, but subsequent names in the
-- list are preserved and the import is not aborted).
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
import qualified Data.ByteString.Char8 as BC
import System.FilePath ((<.>))

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
            -- @ExplicitNamespaces@: export list entries may be prefixed with
            -- @type@ (e.g. @type (~)@, @type Foo@) to disambiguate type-level
            -- names from term-level names.  The namespace prefix is not
            -- semantically meaningful for our resolver, so just swallow the
            -- @type@ token and re-run the dispatch on whatever follows.
            TkTypeKw -> go acc cur1
            TkIdent n   -> go (ExportName n : acc) cur1
            -- MagicHash primop identifiers like `unpackCString#` are
            -- valid export names in modules that enable MagicHash.
            TkPrimId n  -> go (ExportName n : acc) cur1
            -- `module Foo.Bar` re-export form.
            TkModule -> do
                (mMod, cur2) <- parseDottedName src cur1
                case mMod of
                    Just modN -> go (ExportModule modN : acc) cur2
                    Nothing   -> pure (ExportList (reverse acc), cur1)
            TkConId n -> do
                -- Check for Tree(..) or Tree(a,b), but also handle
                -- qualified export names like B.fromStrict or B.ByteString.
                -- A ConId immediately followed by '.' (no space) means this
                -- is a qualified export — advance past 'ConId.bare' and
                -- record the bare name.
                let (peek, curP) = nextSigTok src cur1
                case tkKind peek of
                    TkLParen -> do
                        (subs, cur2) <- parseExportSubs src curP
                        go (ExportType n (Just subs) : acc) cur2
                    TkDot -> do
                        -- Qualified export: <ConId>.<bare> or <ConId>.<ConId>.
                        -- The dot is a qualifier separator only when it abuts
                        -- the ConId (no whitespace) — Haskell 2010 §5.2 / §2.4.
                        -- GHC rejects @module M (B . bar) where@ as a parse
                        -- error, so we must NOT silently rewrite that form
                        -- to the qualified export of @bar@.
                        let (dotTok, curAfterDot) = nextToken src cur1
                            (conTok, _)            = nextSigTok src cur
                            abuts = tkStart dotTok == tkEnd conTok
                        case tkKind dotTok of
                            TkDot
                                | abuts -> do
                                    let (bareTok, curAfterBare) = nextToken src curAfterDot
                                    case tkKind bareTok of
                                        TkIdent bare -> do
                                            -- qualified lower-case export (e.g. B.fromStrict)
                                            go (ExportName bare : acc) curAfterBare
                                        TkConId bare -> do
                                            -- qualified upper-case export (e.g. B.ByteString)
                                            let (peek2, curP2) = nextSigTok src curAfterBare
                                            case tkKind peek2 of
                                                TkLParen -> do
                                                    (subs, cur3) <- parseExportSubs src curP2
                                                    go (ExportType bare (Just subs) : acc) cur3
                                                _ -> go (ExportType bare Nothing : acc) curAfterBare
                                        _ ->
                                            -- Unrecognized qualified form: emit bare ConId
                                            go (ExportType n Nothing : acc) cur1
                            _ -> go (ExportType n Nothing : acc) cur1
                    _ -> go (ExportType n Nothing : acc) cur1
            TkEof    -> pure (ExportList (reverse acc), cur1)
            -- Parenthesised operator export: @(++)@, @(!)@, @(@?=@), etc.
            -- Peek inside the parens to extract the operator name so
            -- that 'exportsName' can match it.  Only a single token is
            -- expected inside; anything more complex is skipped safely.
            TkLParen -> do
                let (inner, cur2) = nextSigTok src cur1
                case operatorTokenName (tkKind inner) of
                    Just op -> do
                        -- Consume the closing paren if present.
                        let (close, cur3) = nextSigTok src cur2
                        case tkKind close of
                            TkRParen -> go (ExportName op : acc) cur3
                            _        -> go (ExportName op : acc) cur2
                    Nothing -> case tkKind inner of
                        -- '@'-prefixed operator: @?=, @=?, etc.
                        -- '@' is TkAt (not isOpChar), so @?= is TkAt + TkSymOp "?=".
                        -- Reconstruct the full operator name by prepending "@".
                        TkAt -> do
                            let (rest, cur3) = nextSigTok src cur2
                            let fullOp = case tkKind rest of
                                    TkSymOp suf -> BC.pack "@" <> suf
                                    _           -> BC.pack "@"
                            let curAfterOp = case tkKind rest of
                                    TkSymOp _ -> cur3
                                    _         -> cur2
                            let (close, cur4) = nextSigTok src curAfterOp
                            case tkKind close of
                                TkRParen -> go (ExportName fullOp : acc) cur4
                                _        -> go (ExportName fullOp : acc) curAfterOp
                        -- Anything else (e.g. section or complex form): skip
                        -- the whole group and continue.
                        _ -> do
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
                -- Operator in parens inside subs: Class((>>=), (>>), return).
                -- Skip the parenthesised operator and continue.
                TkLParen  -> do
                    cAfterOp <- skipToCloseParen s c1 1
                    loop subs cAfterOp
                -- Bare operator (symbolic or backtick-wrapped): skip it.
                TkSymOp _ -> loop subs c1
                TkBacktick -> do
                    -- Skip `op`
                    let (_op, c2) = nextSigTok s c1
                    let (_bt, c3) = nextSigTok s c2
                    loop subs c3
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
    -- PackageImports: an optional "pkg" string between `qualified` (if
    -- present) and the module name. We accept it unconditionally and
    -- discard the contents — module resolution is by name only.
    let (tPkg, curAfterPkg) = nextSigTok src curQ
    let curName = case tkKind tPkg of
            TkStr _ -> curAfterPkg
            _       -> curQ
    (mModName, cur2) <- parseDottedName src curName
    case mModName of
        Nothing   -> pure Nothing
        Just modN -> do
            -- Optional: as Alias
            let (t2, cur3) = nextSigTok src cur2
            (alias, cur4) <- case tkKind t2 of
                -- 'as' is a soft keyword (emitted as TkIdent "as" by the lexer);
                -- match it by string value here in the import-header context.
                TkIdent "as" -> do
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
            -- MagicHash primop identifiers (e.g. `Addr#`, `unpackCString#`)
            -- must be accepted in an explicit import list, same as in
            -- parseExportList.  Before this, hitting a TkPrimId bailed
            -- the list mid-stream and lost every subsequent import
            -- declaration (seen in Data.Text.Show's `import GHC.Exts
            -- (Ptr(..), Int(..), Addr#, indexWord8OffAddr#)`).
            TkPrimId n -> nameOrSub acc n cur1
            TkEof    -> pure (reverse acc, cur1)
            -- @(#.)@ and similar: the lexer emits @(#@ as TkLUnbox
            -- (unboxed-tuple open), then @.@ and @)@.  Treat this
            -- shape as an operator-group open and capture the inner
            -- operator name.  If it doesn't close cleanly just skip.
            TkLUnbox -> do
                let (inner, cur2) = nextSigTok src cur1
                case tkKind inner of
                    TkSymOp op -> do
                        let (close, cur3) = nextSigTok src cur2
                        let cur' = case tkKind close of
                                TkRParen -> cur3
                                _        -> cur2
                        go (BC.pack "#" <> op : acc) cur'
                    _ -> do
                        -- Unknown — skip until we see TkRParen at depth 0.
                        curSkip <- skipToCloseParen src cur1 1
                        go acc curSkip
            -- Operator import: `(++)`, `(!)`, `(.&.)`, `(@?=)`, etc.
            -- Extract the operator name from the (op) group so that
            -- specAllows can match it during resolveImport.
            TkLParen -> do
                let (inner, cur2) = nextSigTok src cur1
                case operatorTokenName (tkKind inner) of
                    Just op -> do
                        let (close, cur3) = nextSigTok src cur2
                        let cur' = case tkKind close of
                                TkRParen -> cur3
                                _        -> cur2
                        go (op : acc) cur'
                    Nothing -> case tkKind inner of
                        -- '@'-prefixed operator: (@?=), (@=?), etc.
                        TkAt -> do
                            let (rest, cur3) = nextSigTok src cur2
                            let (fullOp, curAfterOp) = case tkKind rest of
                                    TkSymOp suf -> (BC.pack "@" <> suf, cur3)
                                    _           -> (BC.pack "@", cur2)
                            let (close, cur4) = nextSigTok src curAfterOp
                            let cur' = case tkKind close of
                                    TkRParen -> cur4
                                    _        -> curAfterOp
                            go (fullOp : acc) cur'
                        -- Unknown/complex inner form: skip whole group.
                        _ -> do
                            curSkip <- skipToCloseParen src cur1 1
                            go acc curSkip
            _        -> pure (reverse acc, cur)

    -- After a name, check for a `(..)` or `(sub, sub)` tail, and skip it.
    nameOrSub acc n cur = do
        let (peek, curP) = nextSigTok src cur
        case tkKind peek of
            TkLParen -> do
                -- @Type(..)@ or @Type(Ctor1, Ctor2)@ in an import list:
                -- previously we just fast-forwarded through the paren
                -- group, dropping every constructor name on the floor.
                -- Now: parse the contents as well.
                --
                --   * For an explicit list @Type(Ctor1, Ctor2)@, every
                --     identifier becomes its own entry in the import list
                --     so 'specAllows' will accept @Ctor1@ etc.
                --   * For @Type(..)@ we have no way to enumerate the
                --     constructors at parse time (the owning module
                --     isn't loaded yet), so we leave a sentinel
                --     @"$dotdot:<Type>"@ that 'specAllows' (and the
                --     entry-env builder) recognise to mean "any
                --     constructor of @Type@".
                (subs, curEnd) <- parseSubNames src curP
                let typedSubs =
                        [ if sub == BC.pack "$dotdot"
                            then BC.pack "$dotdot:" <> n
                            else sub
                        | sub <- subs
                        ]
                go (reverse (n : typedSubs) ++ acc) curEnd
            _ -> go (n : acc) cur

    -- Parse the contents of a @(...)@ trailing a name in an import list.
    -- Returns the list of inner names (or sentinels for @(..)@) and the
    -- cursor just past the closing paren.  Already AT the @(@.
    parseSubNames :: Source -> Cursor -> IO ([ByteString], Cursor)
    parseSubNames s cur0 = sub [] cur0
      where
        sub acc cur = do
            let (tok, cur1) = nextSigTok s cur
            case tkKind tok of
                TkRParen -> pure (reverse acc, cur1)
                TkComma  -> sub acc cur1
                TkConId nm -> sub (nm : acc) cur1
                TkIdent nm -> sub (nm : acc) cur1
                TkPrimId nm -> sub (nm : acc) cur1
                TkDotDot -> sub (BC.pack "$dotdot" : acc) cur1
                TkDot -> do
                    -- @..@ — promote to wildcard sentinel.  Two TkDot
                    -- tokens land here back-to-back; consume the
                    -- second.
                    let (tok2, cur2) = nextSigTok s cur1
                    case tkKind tok2 of
                        TkDot -> sub (BC.pack "$dotdot" : acc) cur2
                        _     -> sub acc cur1
                TkEof -> pure (reverse acc, cur1)
                -- Operator-group inside an exposed-ctor list:
                -- @Type(Op1, (:|), (+))@ shapes up the same as the
                -- top-level @ImportList@ TkLParen branch — extract the
                -- inner op via 'operatorTokenName', consume the
                -- matching @)@, and continue.  Without this, the
                -- catch-all below would advance past only the inner @(@
                -- and then return at the FIRST @)@ it saw (the op's
                -- closing paren), leaving the cursor one paren shallow
                -- and corrupting every subsequent import parse — the
                -- exact failure mode behind 'NonEmpty ((:|))' on
                -- bytestring's @Data.ByteString.Internal.Type@ line 149,
                -- which silently dropped every @import GHC.ForeignPtr@
                -- below it.
                TkLParen -> do
                    let (inner, cur2) = nextSigTok s cur1
                    case operatorTokenName (tkKind inner) of
                        Just op -> do
                            let (close, cur3) = nextSigTok s cur2
                            let cur' = case tkKind close of
                                    TkRParen -> cur3
                                    _        -> cur2
                            sub (op : acc) cur'
                        Nothing -> do
                            -- Unknown inner form: balance-skip to
                            -- matching @)@ so we don't bleed past
                            -- the import list's terminator.
                            curSkip <- skipToCloseParen s cur1 1
                            sub acc curSkip
                _     -> sub acc cur1

operatorTokenName :: TokenKind -> Maybe ByteString
operatorTokenName = \case
    TkPlus     -> Just (BC.pack "+")
    TkPlusPlus -> Just (BC.pack "++")
    TkMinus    -> Just (BC.pack "-")
    TkStar     -> Just (BC.pack "*")
    TkEqEq     -> Just (BC.pack "==")
    TkNeq      -> Just (BC.pack "/=")
    TkLt       -> Just (BC.pack "<")
    TkLe       -> Just (BC.pack "<=")
    TkGt       -> Just (BC.pack ">")
    TkGe       -> Just (BC.pack ">=")
    TkAnd      -> Just (BC.pack "&&")
    TkOr       -> Just (BC.pack "||")
    TkColon    -> Just (BC.pack ":")
    TkDot      -> Just (BC.pack ".")
    TkBang     -> Just (BC.pack "!")
    TkDollar   -> Just (BC.pack "$")
    TkSymOp n  -> Just n
    _          -> Nothing

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
