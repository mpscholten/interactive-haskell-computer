-- | On-demand symbol finder. The core of demand-driven laziness:
-- given a target name, advance the lexer through top-level tokens
-- until we find @name [param*] = ...@, returning a list of clauses
-- (each with a params-span and a body-span).
--
-- Never parses a body. Never materializes a token list. Stops as soon
-- as the requested name is fully collected (all consecutive clauses of
-- that name at column 1 have been absorbed into a single 'BindingLhs').
--
-- Phase 2.2.5 LHS grammar:
--
-- @
-- top      ::= clause ( clause* with same name)
-- clause   ::= ident pattern* ('=' rhs | ('|' guard '=' rhs)+)
-- @
--
-- 'findBinding' returns a 'BindingLhs' whose 'lhsClauses' has one
-- entry per equation. Pattern-parsing and guard-parsing are deferred
-- to 'IHC.Parser'; the scanner only records byte spans.
module IHC.Scan
    ( KnownSymbols
    , emptyKnownSymbols
    , SymbolInfo(..)
    , BindingLhs(..)
    , Clause(..)
    , findBinding
    , lookupSymbol
    , markCompiled
    , scanAllTopLevelNames
      -- * Data declarations
    , DataRegistry
    , FieldRegistry
    , scanDataDecls
      -- * Instance declarations
    , InstanceDecl(..)
    , scanInstanceDecls
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.IORef
import Foreign.Ptr (Ptr)

import IHC.Lexer
import IHC.Source

-- | What we know about a top-level name.
data SymbolInfo
    = SpanOnly !BindingLhs      -- ^ skimmed past; clause locations known
    | Compiled !(Ptr ())        -- ^ already JITted; entry pointer
    deriving (Eq, Show)

-- | One equation in a (possibly multi-clause) binding.
--
-- @clausePats@ is the byte span containing the parameter patterns
-- between the binder-name and either the @=@ (plain body) or the first
-- @|@ (guarded body). May be an empty span (no params).
--
-- @clauseRhs@ is the byte span containing the right-hand side:
-- either @expr@ (for a plain body) or @| g1 = e1 | g2 = e2 ...@
-- (for guards). The parser detects which by peeking the first token.
data Clause = Clause
    { clausePats :: !Span
    , clauseRhs  :: !Span
    } deriving (Eq, Show)

data BindingLhs = BindingLhs
    { lhsClauses :: ![Clause]
    } deriving (Eq, Show)

type KnownSymbols = IORef (Map ByteString SymbolInfo, Cursor)

emptyKnownSymbols :: IO KnownSymbols
emptyKnownSymbols = newIORef (Map.empty, startCursor)

lookupSymbol :: KnownSymbols -> ByteString -> IO (Maybe SymbolInfo)
lookupSymbol ref name = do
    (m, _) <- readIORef ref
    pure (Map.lookup name m)

markCompiled :: KnownSymbols -> ByteString -> Ptr () -> IO ()
markCompiled ref name ptr =
    modifyIORef' ref (\(m, c) -> (Map.insert name (Compiled ptr) m, c))

-- | Scan the entire source file and return the names of all top-level
-- value bindings (column-1 lowercase identifiers with an @=@ or guards).
-- Type signatures, data declarations, and class/instance declarations are
-- skipped.  This is used by the REPL import machinery to enumerate every
-- exported name before demand-driving their bodies.
scanAllTopLevelNames :: Source -> IO [ByteString]
scanAllTopLevelNames src = go [] startCursor
  where
    go acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof     -> pure (reverse acc)
            TkNewline -> go acc cur'
            TkIdent name
                | tkCol tok == 1 -> do
                    -- Quick-scan past this binding's clauses so we don't
                    -- re-report the same name from a continuation clause.
                    curSkipped <- skipThroughBinding src name cur'
                    let acc' = if name `elem` acc then acc else name : acc
                    go acc' curSkipped
            _ -> go acc cur'

-- | Skip through all consecutive clauses of the given binding name so
-- that the scanner doesn't report it multiple times.
skipThroughBinding :: Source -> ByteString -> Cursor -> IO Cursor
skipThroughBinding src name cur0 = do
    mClause <- scanOneClauseAfterName src cur0
    case mClause of
        Nothing          -> pure cur0
        Just (_, curAfter) -> do
            -- Check whether the next column-1 significant token is the
            -- same name (another clause). If so, consume that too.
            let (peek, peekAfter) = peekSigTokFrom src curAfter
            case tkKind peek of
                TkIdent n | n == name && tkCol peek == 1 ->
                    skipThroughBinding src name peekAfter
                _ -> pure curAfter

-- | 'peekSigTokFrom' — skip newlines and return the next non-newline token
-- at the given cursor without actually advancing the returned cursor past it.
peekSigTokFrom :: Source -> Cursor -> (Token, Cursor)
peekSigTokFrom src cur =
    let (tok, cur') = nextToken src cur
    in case tkKind tok of
        TkNewline -> peekSigTokFrom src cur'
        _         -> (tok, cur')

-- | Advance looking for a top-level binding named @target@. Returns
-- the binding's LHS (list of clauses) if found. Consecutive equations
-- at column 1 with the same name are grouped into a single 'BindingLhs'.
findBinding :: Source -> KnownSymbols -> ByteString -> IO (Maybe BindingLhs)
findBinding src ref target = do
    existing <- lookupSymbol ref target
    case existing of
        Just (SpanOnly lhs) -> pure (Just lhs)
        Just (Compiled _)   -> pure Nothing
        Nothing -> do
            (m0, c0) <- readIORef ref
            go m0 c0
  where
    go acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof -> do
                writeIORef ref (acc, cur')
                pure Nothing
            TkNewline -> go acc cur'
            TkIdent name
                | tkCol tok == 1 -> handleTopIdent acc name tok cur'
                | otherwise      -> go acc cur'
            _ -> go acc cur'

    -- We saw `name` at column 1. Scan through its params+rhs to find
    -- the clause boundaries, then peek ahead: if the next column-1
    -- significant token is the same name, accumulate another clause.
    handleTopIdent acc name startTok cur = do
        mClause <- scanOneClauseAfterName src cur
        case mClause of
            Nothing      -> skipBadBinding acc startTok cur
            Just (clause, curAfter) -> do
                (moreClauses, curFinal) <-
                    collectMoreClauses name [clause] curAfter
                let lhs  = BindingLhs (reverse moreClauses)
                    acc' = Map.insert name (SpanOnly lhs) acc
                if name == target
                    then do
                        writeIORef ref (acc', curFinal)
                        pure (Just lhs)
                    else go acc' curFinal

    -- After one clause body ends (at a column-1 boundary), peek to see
    -- if the same binder-name repeats. If so, consume its params+rhs
    -- and loop. Otherwise, return.
    collectMoreClauses name acc cur = do
        -- Skip newlines only; do NOT skip other content.
        let (tok, curAfter) = peekSigTok cur
        case tkKind tok of
            TkIdent n | n == name && tkCol tok == 1 -> do
                mClause <- scanOneClauseAfterName src curAfter
                case mClause of
                    Nothing -> pure (acc, cur)
                    Just (cl, curNext) ->
                        collectMoreClauses name (cl : acc) curNext
            _ -> pure (acc, cur)

    -- Peek the next significant (non-newline) token without losing
    -- the ability to restart from `cur` on backtrack. Returns (token,
    -- cursor AFTER the token) — but only used for its kind/column;
    -- the body-end cursor is what advances.
    peekSigTok cur0 =
        let (tok, curN) = nextToken src cur0 in
        case tkKind tok of
            TkNewline -> peekSigTok curN
            _         -> (tok, curN)

    skipBadBinding acc startTok cur =
        let eolPos = findLineEnd src (cPos cur)
            cur'   = Cursor eolPos (tkLine startTok) 1
        in go acc cur'

-- | Starting just after a binder-name identifier, scan the parameter
-- patterns and the RHS until this clause ends (body extends through
-- all indented continuation lines, up to the next column-1 token or
-- EOF). Returns a 'Clause' (spans for patterns + rhs) and the cursor
-- positioned at the end of the clause body.
--
-- The clause is delimited as:
--   patsSpan = [afterName .. startOfFirstEqOrBar)
--   rhsSpan  = [startOfFirstEqOrBar .. clauseEnd)
--   clauseEnd is the body-end (last newline before the next col-1 start,
--   or EOF, whichever comes first).
--
-- If we fail to find either @=@ or @|@ on this line, returns Nothing so
-- the caller can recover.
scanOneClauseAfterName :: Source -> Cursor -> IO (Maybe (Clause, Cursor))
scanOneClauseAfterName src curAfterName = do
    let patsStart = cPos (skipTrivia src curAfterName)
    mEqOrBar <- findEqOrBarOnLine src curAfterName
    case mEqOrBar of
        Nothing -> pure Nothing
        Just (sepTokStart, cur1) -> do
            -- RHS starts at sepTokStart (the `=` or `|` character).
            -- Body extends to the next column-1 significant position.
            let bodyStart = sepTokStart
                bodyEnd   = findBodyEnd src bodyStart
                patsEnd   = sepTokStart
                clause    = Clause (patsStart, patsEnd) (bodyStart, bodyEnd)
                curAfter  = Cursor bodyEnd 0 1
            pure (Just (clause, curAfter))

-- | Scan forward from @cur@ collecting tokens until we encounter
-- either a top-level @=@ or a top-level @|@ (not inside parens or
-- brackets). Returns the byte offset of that token and the cursor
-- just past it. Returns Nothing if the line ends or a newline-then-
-- col-1 boundary appears first (malformed LHS — e.g. a bare binder
-- with no RHS).
--
-- Nested parens/brackets/braces are tracked so an @=@ inside a record
-- literal (not supported yet, but future-proof) or a @|@ inside a
-- bracketed expression isn't mistaken for the RHS separator.
findEqOrBarOnLine :: Source -> Cursor -> IO (Maybe (Pos, Cursor))
findEqOrBarOnLine src cur0 = go cur0 (0 :: Int)
  where
    go cur depth = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof     -> pure Nothing
            TkEq      | depth == 0 -> pure (Just (tkStart tok, cur'))
            TkBar     | depth == 0 -> pure (Just (tkStart tok, cur'))
            TkLParen  -> go cur' (depth + 1)
            TkLBracket-> go cur' (depth + 1)
            TkLBrace  -> go cur' (depth + 1)
            TkRParen  -> go cur' (max 0 (depth - 1))
            TkRBracket-> go cur' (max 0 (depth - 1))
            TkRBrace  -> go cur' (max 0 (depth - 1))
            TkNewline ->
                -- If the next significant token is at col 1, the LHS
                -- never got its '=' — malformed. Stop.
                let (nxt, _) = nextToken src cur' in
                case tkKind nxt of
                    TkEof -> pure Nothing
                    _ | tkCol nxt == 1 -> pure Nothing
                      | otherwise      -> go cur' depth
            _ -> go cur' depth

-- | Scan until the end of the current line (or EOF). Used only for
-- recovering after a malformed LHS; the real body-extent logic is
-- 'findBodyEnd'.
findLineEnd :: Source -> Pos -> Pos
findLineEnd s p = case peekByte s p of
    Nothing   -> p
    Just 0x0A -> p
    Just 0x0D -> p
    Just _    -> findLineEnd s (p + 1)

-- | A body extends until either EOF or the next column-1 non-whitespace
-- byte (the start of the next top-level binding). Indented continuation
-- lines and blank lines are part of the body.
findBodyEnd :: Source -> Pos -> Pos
findBodyEnd s = scanBody
  where
    -- Keep scanning bytes; at each newline, decide whether the body
    -- continues (next line is indented or blank) or ends.
    scanBody p = case peekByte s p of
        Nothing   -> p
        Just 0x0A -> checkNext (p + 1) p
        Just 0x0D -> checkNext (p + 1) p
        Just _    -> scanBody (p + 1)

    checkNext q lastNl = case peekByte s q of
        Nothing   -> lastNl        -- EOF terminates body.
        Just 0x20 -> scanBody q    -- space: indented continuation, keep going.
        Just 0x09 -> scanBody q    -- tab: same.
        Just 0x0A -> checkNext (q + 1) q   -- blank line, check after.
        Just 0x0D -> checkNext (q + 1) q
        Just _    -> lastNl        -- col-1 content: body ends at last newline.

--------------------------------------------------------------------------------
-- Data declarations
--------------------------------------------------------------------------------

-- | Map from constructor name to arity. Populated once per program by
-- 'scanDataDecls', consumed by 'IHC.Builtins.buildConEnv'.
type DataRegistry = Map ByteString Int

-- | Map from record-field name to a list of @(constructor name, field index)@
-- pairs. Used by 'IHC.Builtins.buildFieldEnv' to generate field-accessor
-- functions automatically.
type FieldRegistry = Map ByteString [(ByteString, Int)]

-- | Scan the whole source for top-level @data@ declarations and
-- collect every (constructor, arity) pair. This is a lexer-only pass —
-- we do NOT build any AST, don't track type parameters, and don't care
-- about the LHS (@data Maybe a@ vs @data Tree@). We just walk
-- constructor alternatives separated by @|@.
--
-- Grammar handled:
--
-- @
-- data TyCon tyvar* = Con0 field* ( | ConN field* )*
-- @
--
-- where each @field@ is either an atom (TkConId / TkIdent) or a
-- parenthesised group counted as a single field.
scanDataDecls :: Source -> IO DataRegistry
scanDataDecls src = go Map.empty startCursor
  where
    go !acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof -> pure acc
            TkData | tkCol tok == 1 -> do
                (acc', curAfter) <- scanOneDataDecl acc cur'
                go acc' curAfter
            _ -> go acc cur'

    -- Parse: (TkConId tyvar*) '=' ctor ('|' ctor)*  until the decl ends.
    -- Decl ends at a column-1 token (next top-level binding / data) or
    -- at EOF. TkNewline is trivia.
    scanOneDataDecl !acc cur0 = do
        -- Skip tyname + tyvars.
        curHeader <- skipUntilEq cur0
        -- Now collect ctors separated by '|'.
        collectCtors acc curHeader

    skipUntilEq cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEq   -> pure cur'
            TkEof  -> pure cur
            _      -> skipUntilEq cur'

    -- At start of each ctor: expect TkConId, then consume field atoms
    -- until TkBar / decl-end.
    collectCtors !acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkNewline -> collectCtors acc cur'
            TkConId name -> do
                (arity, curN) <- countCtorFields 0 cur'
                let acc' = Map.insert name arity acc
                -- After fields, check for '|' (more ctors) or decl end.
                let (sep, curSep) = nextToken src curN
                case tkKind sep of
                    TkBar    -> collectCtors acc' curSep
                    TkNewline -> collectCtors acc' curSep
                    _        -> pure (acc', curN)
            -- Missing constructor (malformed) or decl ended.
            _ -> pure (acc, cur)

    -- Count atoms up to '|', column-1 token, or EOF.
    countCtorFields !n cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkBar            -> pure (n, cur)             -- don't consume
            TkEof            -> pure (n, cur)
            TkNewline        ->
                -- If the next significant token is at col 1, decl ends;
                -- otherwise it's whitespace between fields.
                let (peek, _) = nextToken src cur' in
                case tkKind peek of
                    TkEof -> pure (n, cur')
                    _ | tkCol peek == 1 -> pure (n, cur')
                      | otherwise       -> countCtorFields n cur'
            TkLParen -> do
                curAfter <- skipToMatchingRParen 1 cur'
                countCtorFields (n + 1) curAfter
            TkConId _ -> countCtorFields (n + 1) cur'
            TkIdent _ -> countCtorFields (n + 1) cur'
            -- Unrecognized token stops the field scan gracefully.
            _ -> pure (n, cur)

    skipToMatchingRParen :: Int -> Cursor -> IO Cursor
    skipToMatchingRParen !depth cur
        | depth <= 0 = pure cur
        | otherwise = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkLParen -> skipToMatchingRParen (depth + 1) cur'
                TkRParen -> skipToMatchingRParen (depth - 1) cur'
                TkEof    -> pure cur'
                _        -> skipToMatchingRParen depth cur'

--------------------------------------------------------------------------------
-- Instance declarations
--------------------------------------------------------------------------------

-- | One top-level @instance C T where method = body@ declaration.
-- 'instClassName' is the class name, 'instTypeName' is the head type
-- (first uppercase identifier after the class name, before @where@).
-- 'instMethods' is a list of (method-name, Clause-list) pairs exactly
-- like the output of 'findBinding'.
data InstanceDecl = InstanceDecl
    { instClassName :: !ByteString
    , instTypeName  :: !ByteString
    , instMethods   :: ![(ByteString, BindingLhs)]
    } deriving (Eq, Show)

-- | Scan the whole source for top-level @instance@ declarations and
-- return each one as an 'InstanceDecl'. The method bodies are recorded
-- as 'Clause' byte-spans (same format as 'findBinding') so the caller
-- can hand them to the parser.
--
-- Grammar handled (permissive):
-- @
-- instance [ctx =>] ClassName TypeHead where
--   method [params] = body
--   ...
-- @
-- The instance head (everything between @instance@ and @where@) is
-- partially parsed: we grab the first uppercase identifier as the class
-- name and walk for the first upper-or-lower identifier that follows a
-- constraint arrow or is the head type.
scanInstanceDecls :: Source -> IO [InstanceDecl]
scanInstanceDecls src = go [] startCursor
  where
    go !acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof -> pure (reverse acc)
            TkInstance | tkCol tok == 1 -> do
                mDecl <- scanOneInstance cur'
                case mDecl of
                    Nothing   -> go acc cur'
                    Just decl -> go (decl : acc) cur'
            _ -> go acc cur'

    -- After `instance`, scan the head to find class name + type name,
    -- then scan the `where` body for method bindings.
    scanOneInstance cur0 = do
        -- Collect tokens up to `where`.
        (mClassName, mTypeName, curWhere) <- parseInstanceHead cur0
        case (mClassName, mTypeName) of
            (Just cls, Just typ) -> do
                methods <- parseInstanceBody curWhere
                pure (Just (InstanceDecl cls typ methods))
            _ -> pure Nothing

    -- Parse: [context =>] ClassName TypeHead where
    -- We grab the first TkConId as class name, then look for the type.
    -- Simplified: first ConId = class, next ConId/Ident after `=>` or
    -- directly = type. Stop at `where`.
    parseInstanceHead cur0 = scanHead cur0 Nothing Nothing False
      where
        scanHead cur mCls mTyp seenArrow = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkEof    -> pure (mCls, mTyp, cur)
                TkWhere  -> pure (mCls, mTyp, cur')
                TkNewline -> scanHead cur' mCls mTyp seenArrow
                TkDArrow -> scanHead cur' mCls Nothing True
                TkConId n ->
                    case mCls of
                        Nothing -> scanHead cur' (Just n) mTyp seenArrow
                        Just _  ->
                            case mTyp of
                                Nothing -> scanHead cur' mCls (Just n) seenArrow
                                Just _  -> scanHead cur' mCls mTyp seenArrow
                TkIdent _ ->
                    -- Lower-case type variable or instance argument like
                    -- `instance Eq a => Eq [a]` — skip for now, the
                    -- bracket-balanced scan below handles more complex heads.
                    scanHead cur' mCls mTyp seenArrow
                TkLParen -> do
                    -- Skip parenthesised types like `(,)` or `(a, b)`.
                    curAfter <- skipParens 1 cur'
                    -- The head type might BE a tuple or list; extract its name.
                    let headTyp = case mTyp of
                            Nothing -> Just (BC.pack "(,)")
                            Just _  -> mTyp
                    scanHead curAfter mCls headTyp seenArrow
                TkLBracket -> do
                    curAfter <- skipBrackets 1 cur'
                    let headTyp = case mTyp of
                            Nothing -> Just (BC.pack "[]")
                            Just _  -> mTyp
                    scanHead curAfter mCls headTyp seenArrow
                _ -> scanHead cur' mCls mTyp seenArrow

    skipParens :: Int -> Cursor -> IO Cursor
    skipParens !d cur
        | d <= 0    = pure cur
        | otherwise = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkLParen -> skipParens (d + 1) cur'
                TkRParen -> skipParens (d - 1) cur'
                TkEof    -> pure cur'
                _        -> skipParens d cur'

    skipBrackets :: Int -> Cursor -> IO Cursor
    skipBrackets !d cur
        | d <= 0    = pure cur
        | otherwise = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkLBracket -> skipBrackets (d + 1) cur'
                TkRBracket -> skipBrackets (d - 1) cur'
                TkEof      -> pure cur'
                _          -> skipBrackets d cur'

    -- Parse the `where` body: a layout block of method bindings.
    -- Each binding is `methodName [pats] = body` (possibly multi-clause).
    -- We reuse the same clause-scanning logic as findBinding.
    parseInstanceBody cur0 = scanMethods Map.empty cur0

    scanMethods !acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof     -> pure (Map.toList acc)
            TkNewline -> scanMethods acc cur'
            TkIdent name | tkCol tok > 1 -> do
                mClause <- scanOneClauseAfterName src cur'
                case mClause of
                    Nothing -> scanMethods acc cur'
                    Just (clause, curNext) -> do
                        (moreClauses, curFinal) <-
                            collectInstanceClauses name [clause] curNext
                        let lhs  = BindingLhs (reverse moreClauses)
                            acc' = Map.insert name lhs acc
                        scanMethods acc' curFinal
            -- Stop when we hit a new column-1 token (next top-level decl).
            _ | tkCol tok == 1 && tkKind tok /= TkNewline -> pure (Map.toList acc)
              | otherwise -> scanMethods acc cur'

    collectInstanceClauses name acc cur = do
        let (tok, curAfter) = peekSig cur
        case tkKind tok of
            TkIdent n | n == name && tkCol tok > 1 -> do
                mClause <- scanOneClauseAfterName src curAfter
                case mClause of
                    Nothing -> pure (acc, cur)
                    Just (cl, curNext) ->
                        collectInstanceClauses name (cl : acc) curNext
            _ -> pure (acc, cur)

    peekSig cur0 =
        let (tok, curN) = nextToken src cur0 in
        case tkKind tok of
            TkNewline -> peekSig curN
            _         -> (tok, curN)
