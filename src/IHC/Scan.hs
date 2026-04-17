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
    , TypeCtorRegistry
    , scanDataDecls
      -- * Deriving synthesis
    , FunctorFieldRole(..)
    , FunctorCtor(..)
    , FunctorDerivDecl(..)
    , scanFunctorDerivings
      -- * Instance declarations
    , InstanceDecl(..)
    , scanInstanceDecls
      -- * Class declarations
    , ClassDecl(..)
    , scanClassDecls
      -- * Phase 3.2 + 3.4: type family / type instance skip helper
    , skipTypeDecl
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.IORef
import Control.Monad (when)
import Debug.Trace (traceIO)
import Foreign.Ptr (Ptr)

import IHC.Classes (normalizeTyTag)
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
                    -- Check if this is an infix binding: `arg \`op\` arg = ...`,
                    -- `arg @op arg = ...`, or `arg |> arg = ...`. If so, report
                    -- the operator name, not the first-argument name.
                    let bindName = case peekInfixOp src cur' of
                                       Just op -> op
                                       Nothing -> name
                    -- Quick-scan past this binding's clauses so we don't
                    -- re-report the same name from a continuation clause.
                    curSkipped <- skipThroughBinding src name cur'
                    let acc' = if bindName `elem` acc then acc else bindName : acc
                    go acc' curSkipped
            -- Prefix operator binding: @(|>) x f = ...@. The @(@ is at col 1,
            -- followed by an operator token and @)@.
            TkLParen | tkCol tok == 1 -> do
                case peekPrefixOpBinding src (Cursor (tkStart tok) (tkLine tok) (tkCol tok)) of
                    Just (opName, curAfterClose) -> do
                        curSkipped <- skipThroughPrefixOpBinding src opName curAfterClose
                        let acc' = if opName `elem` acc then acc else opName : acc
                        go acc' curSkipped
                    Nothing -> go acc cur'
            -- Phase 3.2 + 3.4: explicitly skip top-level type / type family /
            -- type instance declarations (DataKinds + TypeFamilies). These are
            -- not value bindings and must not be reported as names.
            TkTypeKw | tkCol tok == 1 -> go acc (skipTypeDecl src cur')
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

-- | Skip through all consecutive clauses of a prefix-form operator
-- binding @(op) pat* = ...@ so the scanner doesn't re-report the op.
-- Subsequent clauses may repeat the prefix form or switch to the
-- infix form @pat1 op pat2 = ...@.
skipThroughPrefixOpBinding :: Source -> ByteString -> Cursor -> IO Cursor
skipThroughPrefixOpBinding src opName curAfterClose = do
    mClause <- scanOneClauseAfterName src curAfterClose
    case mClause of
        Nothing -> pure curAfterClose
        Just (_, curAfter) -> loopClauses curAfter
  where
    loopClauses cur = do
        let (peek, curAfterPeek) = peekSigTokFrom src cur
        case tkKind peek of
            -- Next clause in prefix form: `(op) ... = ...`
            TkLParen | tkCol peek == 1 ->
                case peekPrefixOpBinding src (Cursor (tkStart peek) (tkLine peek) (tkCol peek)) of
                    Just (op', curAfterClose') | op' == opName -> do
                        mClause' <- scanOneClauseAfterName src curAfterClose'
                        case mClause' of
                            Nothing -> pure cur
                            Just (_, curAfter') -> loopClauses curAfter'
                    _ -> pure cur
            -- Next clause in infix form: `x op y = ...`
            TkIdent _ | tkCol peek == 1 ->
                case peekInfixOp src curAfterPeek of
                    Just op' | op' == opName -> do
                        mClause' <- scanOneClauseAfterName src curAfterPeek
                        case mClause' of
                            Nothing -> pure cur
                            Just (_, curAfter') -> loopClauses curAfter'
                    _ -> pure cur
            _ -> pure cur

-- | 'peekSigTokFrom' — skip newlines and return the next non-newline token
-- at the given cursor without actually advancing the returned cursor past it.
peekSigTokFrom :: Source -> Cursor -> (Token, Cursor)
peekSigTokFrom src cur =
    let (tok, cur') = nextToken src cur
    in case tkKind tok of
        TkNewline -> peekSigTokFrom src cur'
        _         -> (tok, cur')

-- | Given a cursor positioned just after a column-1 identifier, check
-- whether this is an infix binding of the form:
--
--   @arg1 \`op\` arg2 = ...@     — backtick operator
--   @arg1 @?= arg2 = ...@       — @\@@-prefixed symbolic operator
--   @arg1 |> arg2 = ...@        — symbolic operator (user-defined or builtin)
--
-- Returns @Just operatorName@ if the pattern matches, @Nothing@ if
-- this is a plain binding (@f x y = ...@).
--
-- We only detect the simple case where the operator immediately follows
-- the first argument (i.e. @arg1@ is the token right after the col-1
-- ident).  Multi-pattern LHS like @f x \`op\` y = ...@ is not handled
-- here but those are rare in practice.
peekInfixOp :: Source -> Cursor -> Maybe ByteString
peekInfixOp src cur =
    let (t1, c1) = peekSigTokFrom src cur
    in case tkKind t1 of
        -- Immediately followed by backtick: `arg \`op\` ...`
        TkBacktick ->
            let (t2, c2) = peekSigTokFrom src c1
            in case tkKind t2 of
                TkIdent op ->
                    let (t3, _) = peekSigTokFrom src c2
                    in case tkKind t3 of
                        TkBacktick -> Just op
                        _          -> Nothing
                _ -> Nothing
        -- Immediately followed by '@'-prefixed op: `arg @?= ...`
        TkAt ->
            let (t2, _) = peekSigTokFrom src c1
            in case tkKind t2 of
                TkSymOp suf -> Just (BC.pack "@" <> suf)
                _           -> Nothing
        -- Immediately followed by a symbolic operator: `arg1 |> arg2 = ...`.
        -- The op must be followed by something that could be a second argument
        -- (ident, paren-wrapped pattern, literal, etc.) — otherwise this is
        -- not an infix-op binding LHS.
        _ | Just opName <- tokenOpNameBS (tkKind t1)
          , opName /= "-"    -- leading minus in `f (- 1) = ...` patterns etc.
                             -- already never matches here since col-1 ident
                             -- already consumed; keeping the exception is a
                             -- no-op safeguard.
          -> let (t2, _) = peekSigTokFrom src c1
             in case tkKind t2 of
                 TkIdent _    -> Just opName
                 TkConId _    -> Just opName
                 TkLParen     -> Just opName
                 TkLBracket   -> Just opName
                 TkUnderscore -> Just opName
                 TkInt   _    -> Just opName
                 TkStr   _    -> Just opName
                 TkChar  _    -> Just opName
                 _            -> Nothing
        _ -> Nothing

-- | Map an operator-like token kind to its printable operator name.
-- Mirrors 'IHC.Parser.tokenOpName' but returns a 'ByteString' for use
-- inside the scanner (which doesn't depend on Parser).
tokenOpNameBS :: TokenKind -> Maybe ByteString
tokenOpNameBS = \case
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
    TkDollar   -> Just (BC.pack "$")
    TkSymOp n  -> Just n
    _          -> Nothing

-- | Detect a prefix-form operator binding: @(op) pat* = ...@. The cursor
-- must be positioned at the @(@ token start (i.e. just before the opening
-- paren has been consumed).  Returns @Just (opName, cursorAfterCloseParen)@
-- if the opening paren wraps an operator; @Nothing@ otherwise (e.g. a
-- tuple pattern like @(x, y) = ...@).
peekPrefixOpBinding :: Source -> Cursor -> Maybe (ByteString, Cursor)
peekPrefixOpBinding src cur0 =
    let (t1, c1) = nextToken src cur0       -- consume '('
    in case tkKind t1 of
        TkLParen ->
            let (t2, c2) = peekSigTokFrom src c1
            in case tokenOpNameBS (tkKind t2) of
                Just opName ->
                    let (t3, c3) = peekSigTokFrom src c2
                    in case tkKind t3 of
                        TkRParen -> Just (opName, c3)
                        _        -> Nothing
                Nothing -> Nothing
        _ -> Nothing

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
            -- Prefix-form operator binding: @(|>) x f = ...@
            TkLParen | tkCol tok == 1 ->
                case peekPrefixOpBinding src (Cursor (tkStart tok) (tkLine tok) (tkCol tok)) of
                    Just (opName, curAfterClose) ->
                        handlePrefixOp acc opName tok curAfterClose
                    Nothing -> go acc cur'
            -- Phase 3.2 + 3.4: explicitly skip type family / type instance /
            -- type synonym declarations so they are never mistaken for bindings.
            TkTypeKw | tkCol tok == 1 -> go acc (skipTypeDecl src cur')
            _ -> go acc cur'

    -- Found `(op) ...` at column 1. Scan this clause, then collect any
    -- follow-on clauses that refer to the same op — either in prefix
    -- form @(op) ... = ...@ or infix form @arg op arg = ...@.
    handlePrefixOp acc opName startTok curAfterClose = do
        mClause <- scanOneClauseAfterName src curAfterClose
        case mClause of
            Nothing      -> skipBadBinding acc startTok curAfterClose
            Just (clause, curAfter) -> do
                (moreClauses, curFinal) <-
                    collectMoreOpClauses opName [clause] curAfter
                let lhs  = BindingLhs (reverse moreClauses)
                    acc' = Map.insert opName (SpanOnly lhs) acc
                if opName == target
                    then do
                        writeIORef ref (acc', curFinal)
                        pure (Just lhs)
                    else go acc' curFinal

    -- After an operator-binding clause body ends, peek for another
    -- clause of the SAME operator (in either prefix or infix form).
    collectMoreOpClauses opName acc cur = do
        let (tok, _curAfter) = peekSigTok cur
        case tkKind tok of
            -- Another prefix clause `(op) ... = ...`
            TkLParen | tkCol tok == 1 ->
                case peekPrefixOpBinding src (Cursor (tkStart tok) (tkLine tok) (tkCol tok)) of
                    Just (op', curAfterClose') | op' == opName -> do
                        mClause <- scanOneClauseAfterName src curAfterClose'
                        case mClause of
                            Nothing -> pure (acc, cur)
                            Just (cl, curNext) ->
                                collectMoreOpClauses opName (cl : acc) curNext
                    _ -> pure (acc, cur)
            -- Infix clause `arg op arg = ...`
            TkIdent _ | tkCol tok == 1 -> do
                let (identTok, curAfterIdent) = nextToken src cur
                case tkKind identTok of
                    TkIdent _ ->
                        case peekInfixOp src curAfterIdent of
                            Just op' | op' == opName -> do
                                mClause <- scanOneClauseAfterName src curAfterIdent
                                case mClause of
                                    Nothing -> pure (acc, cur)
                                    Just (cl, curNext) ->
                                        let cl' = cl { clausePats =
                                                (tkStart identTok, snd (clausePats cl)) }
                                        in collectMoreOpClauses opName (cl' : acc) curNext
                            _ -> pure (acc, cur)
                    _ -> pure (acc, cur)
            _ -> pure (acc, cur)

    -- We saw `name` at column 1. Scan through its params+rhs to find
    -- the clause boundaries, then peek ahead: if the next column-1
    -- significant token is the same name, accumulate another clause.
    handleTopIdent acc name startTok cur = do
        -- If this col-1 identifier is actually the LHS argument of an infix
        -- binding (e.g. `actual \`shouldBe\` expected = ...`), use the
        -- operator as the real binding name.
        let mInfixOp = peekInfixOp src cur
            realName = case mInfixOp of
                           Just op -> op
                           Nothing -> name
        mClause <- scanOneClauseAfterName src cur
        case mClause of
            Nothing      -> skipBadBinding acc startTok cur
            Just (clause, curAfter) -> do
                -- For infix bindings, extend the pats span leftward to include
                -- the first argument (the col-1 identifier).
                -- E.g. for `actual \`shouldBe\` expected = rhs`, the scanner
                -- starts from after `actual`, so patsStart points at the
                -- backtick.  We need it at `actual` so parsePatsIn collects
                -- both arguments as patterns.
                let clause' = case mInfixOp of
                        Just _ -> clause { clausePats =
                                    (tkStart startTok, snd (clausePats clause)) }
                        Nothing -> clause
                (moreClauses, curFinal) <- case mInfixOp of
                    Just op -> collectMoreOpClauses op [clause'] curAfter
                    Nothing -> collectMoreClauses name [clause'] curAfter
                let lhs  = BindingLhs (reverse moreClauses)
                    acc' = Map.insert realName (SpanOnly lhs) acc
                if realName == target
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
scanOneClauseAfterName src = scanOneClauseAfterNameAtCol src 1

-- | Like 'scanOneClauseAfterName' but uses @minBodyCol@ as the column at
-- or below which a subsequent line terminates the current clause body.
-- For top-level bindings @minBodyCol == 1@ (the default). For methods
-- inside an @instance C T where@ or @class C a where@ block, pass the
-- column at which the method name was found; the body then ends at the
-- next line starting at that column (the next sibling clause / method)
-- or at any smaller column.
scanOneClauseAfterNameAtCol :: Source -> Int -> Cursor -> IO (Maybe (Clause, Cursor))
scanOneClauseAfterNameAtCol src minBodyCol curAfterName = do
    let patsStart = cPos (skipTrivia src curAfterName)
    mEqOrBar <- findEqOrBarOnLine src curAfterName
    case mEqOrBar of
        Nothing -> pure Nothing
        Just (sepTokStart, _cur1) -> do
            -- RHS starts at sepTokStart (the `=` or `|` character).
            -- Body extends to the next col <= minBodyCol significant
            -- position (col 1 for top-level, method-indent for methods).
            let bodyStart = sepTokStart
                bodyEnd   = findBodyEndAtCol src minBodyCol bodyStart
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
findBodyEnd s = findBodyEndAtCol s 1

-- | Like 'findBodyEnd' but terminates the body at the next line whose
-- first non-whitespace byte is at column @<= minCol@. For top-level
-- bindings pass @minCol == 1@. For methods inside an instance/class
-- body pass the column at which the method identifier sits; this makes
-- the body stop at the next sibling clause (same column) rather than
-- greedily swallowing it.
findBodyEndAtCol :: Source -> Int -> Pos -> Pos
findBodyEndAtCol s minCol = scanBody
  where
    -- Keep scanning bytes; at each newline, decide whether the body
    -- continues (next line is indented deeper than minCol or blank) or
    -- ends (next line starts at column <= minCol).
    scanBody p = case peekByte s p of
        Nothing   -> p
        Just 0x0A -> checkNext (p + 1) p 1
        Just 0x0D -> checkNext (p + 1) p 1
        Just _    -> scanBody (p + 1)

    -- @col@ tracks the 1-based column of the current position @q@ on
    -- the new line. We consume leading spaces/tabs, then look at the
    -- first non-whitespace byte: if its column is <= minCol, the body
    -- ends at @lastNl@; otherwise the indented line is a continuation.
    checkNext q lastNl !col = case peekByte s q of
        Nothing   -> lastNl
        Just 0x20 -> checkNext (q + 1) lastNl (col + 1)
        Just 0x09 -> checkNext (q + 1) lastNl (col + 1)   -- treat tab as +1
        Just 0x0A -> checkNext (q + 1) q 1                -- blank line
        Just 0x0D -> checkNext (q + 1) q 1
        Just _
            | col <= minCol -> lastNl                     -- sibling / top-level
            | otherwise     -> scanBody q                 -- deeper indent

--------------------------------------------------------------------------------
-- Phase 3.2 + 3.4: type family / type instance skip helper
--------------------------------------------------------------------------------

-- | Skip an entire top-level @type@ declaration (synonym, family, or
-- instance) after the @type@ keyword has already been consumed. The
-- cursor is positioned just after @type@.
--
-- This covers:
--
--   * @type Foo = Bar@                  — plain synonym
--   * @type family Elem c :: Type@      — open type family
--   * @type instance Elem [a] = a@      — type instance
--   * @type family Foo x where@         — closed type family with equations
--
-- For a closed family, all the indented @where@-equations are consumed
-- so the scanner doesn't accidentally process them as value bindings.
-- We rely on 'findBodyEnd' to skip the whole indented block in one shot.
skipTypeDecl :: Source -> Cursor -> Cursor
skipTypeDecl src cur0 =
    -- Find the first column-1 non-blank, non-newline token (= end of decl).
    -- 'findBodyEnd' uses the same "indented continuation" heuristic as the
    -- binding-body scanner.  We start from cur0 (just after 'type') and
    -- let findBodyEnd run from there.  The result is the byte offset of the
    -- last newline before the next top-level declaration; we wrap it in a
    -- Cursor with dummy line/col (the scanner only uses cPos).
    let bodyEnd = findBodyEnd src (cPos cur0)
    in Cursor bodyEnd 0 1

--------------------------------------------------------------------------------
-- Data declarations
--------------------------------------------------------------------------------

-- | Map from constructor name to @(typeName, arity, declIndex)@.
--
-- * @typeName@ is the LHS type-constructor name of the enclosing @data@ /
--   @newtype@ declaration (empty 'ByteString' if the scanner couldn't
--   identify it — e.g. malformed input).
-- * @arity@ is the number of fields the constructor takes.
-- * @declIndex@ is the 0-based position of this constructor within its
--   own data declaration. @data Color = Red | Green | Blue@ gives Red→0,
--   Green→1, Blue→2. Consumers use this to derive Ord (constructors are
--   ordered by declaration position), matching GHC's derived-Ord
--   semantics.
--
-- Populated once per program by 'scanDataDecls', consumed by
-- 'IHC.Builtins.buildConEnv' (which uses the arity to build the
-- constructor value) and by 'IHC.Builtins.structuralOrdering' (which
-- uses the @(typeName, declIndex)@ pair for cross-constructor Ord).
type DataRegistry = Map ByteString (ByteString, Int, Int)

-- | Map from record-field name to a list of @(constructor name, field index)@
-- pairs. Used by 'IHC.Builtins.buildFieldEnv' to generate field-accessor
-- functions automatically.
--
-- The list-valued codomain is what supports @DuplicateRecordFields@:
-- two different constructors declaring a field of the same name both
-- contribute entries to the same key. @collectCtors@ uses
-- @Map.insertWith (++)@ so no entry is lost. At runtime the accessor
-- built by 'IHC.Builtins.buildFieldEnv' dispatches on the VCon's
-- constructor name — which is the observable type tag — to pick the
-- right index.
type FieldRegistry = Map ByteString [(ByteString, Int)]

-- | Map from type-constructor name to the list of data-constructor names
-- that type declares. Built by 'scanDataDecls' alongside 'DataRegistry'.
-- Consumed by the scheduler's @exportsName@ to resolve the @T(..)@ and
-- @T(Ctor1, Ctor2)@ forms of export lists.
type TypeCtorRegistry = Map ByteString [ByteString]

-- | Scan the whole source for top-level @data@ declarations and collect
-- every (constructor, arity) pair plus record-field metadata.
-- This is a lexer-only pass — we do NOT build any AST, don't track type
-- parameters, and don't care about the LHS type name.
--
-- Grammar handled:
--
-- @
-- data TyCon tyvar* = Con0 field*              -- positional ctor
--                   | ConR { f1 :: T1, ... }    -- record-syntax ctor
-- data TyCon where                             -- GADT form
--   Ctor :: ctx => T1 -> T2 -> TyCon
-- data TyCon = forall a. C a => Ctor T1       -- existential ctor
-- @
scanDataDecls :: Source -> IO (DataRegistry, FieldRegistry, TypeCtorRegistry)
scanDataDecls src = go Map.empty Map.empty Map.empty startCursor
  where
    go !dReg !fReg !tReg cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof -> pure (dReg, fReg, tReg)
            TkData | tkCol tok == 1 -> do
                let mTyName = peekTypeName cur'
                    tyName  = maybe BC.empty id mTyName
                ((dReg', fReg'), curAfter) <- scanOneDataDecl tyName (dReg, fReg) cur'
                let tReg' = recordTypeCtors mTyName dReg dReg' tReg
                go dReg' fReg' tReg' curAfter
            -- Phase 3.6: newtype declarations behave like single-constructor
            -- data declarations for our purposes (constructor name → arity 1).
            TkNewtype | tkCol tok == 1 -> do
                let mTyName = peekTypeName cur'
                    tyName  = maybe BC.empty id mTyName
                ((dReg', fReg'), curAfter) <- scanOneDataDecl tyName (dReg, fReg) cur'
                let tReg' = recordTypeCtors mTyName dReg dReg' tReg
                go dReg' fReg' tReg' curAfter
            -- Phase 3.2 + 3.4: skip top-level type / type family / type instance
            -- declarations. These are discarded (no reduction needed for our
            -- interpreter). The skipTypeDecl helper advances past the entire
            -- declaration body including any 'where' equations.
            TkTypeKw | tkCol tok == 1 ->
                go dReg fReg tReg (skipTypeDecl src cur')
            _ -> go dReg fReg tReg cur'

    -- Peek the type-constructor name from the start of a data/newtype decl.
    -- The type name is the first TkConId after the keyword (skipping any
    -- optional parenthesised context and operator tokens). Returns Nothing
    -- if we can't identify it.
    peekTypeName :: Cursor -> Maybe ByteString
    peekTypeName cur0 =
        let loop c =
                let (tk, c') = nextToken src c
                in case tkKind tk of
                    TkEof     -> Nothing
                    TkConId n -> Just n
                    TkNewline -> loop c'
                    -- Skip a parenthesised context like @data (Eq a) => T ...@
                    -- by scanning until the matching ')'.
                    TkLParen  -> loop (skipParensPure 1 c')
                    _         -> loop c'
        in loop cur0

    -- Skip a parenthesised region starting at depth @d@, returning the
    -- cursor just past the matching ')'. Pure; uses 'nextToken'.
    skipParensPure :: Int -> Cursor -> Cursor
    skipParensPure d c
        | d <= 0    = c
        | otherwise =
            let (tk, c') = nextToken src c
            in case tkKind tk of
                TkEof    -> c
                TkLParen -> skipParensPure (d + 1) c'
                TkRParen -> skipParensPure (d - 1) c'
                _        -> skipParensPure d c'

    -- Given the DataRegistry before and after scanning one data decl,
    -- record the set of constructor names that got added under the type
    -- name @mTyName@.
    recordTypeCtors :: Maybe ByteString
                    -> DataRegistry
                    -> DataRegistry
                    -> TypeCtorRegistry
                    -> TypeCtorRegistry
    recordTypeCtors Nothing       _    _     tReg = tReg
    recordTypeCtors (Just tyName) before after tReg =
        let newCtors = Map.keys (Map.difference after before)
        in if null newCtors
             then tReg
             else Map.insertWith (\new old -> old ++ new) tyName newCtors tReg

    -- Parse: (TkConId tyvar*) '=' ctor ('|' ctor)*  until the decl ends.
    -- Decl ends at a column-1 token (next top-level binding / data) or
    -- at EOF. TkNewline is trivia.
    -- Also handles GADT form: TkWhere after the type name.
    --
    -- @tyName@ is the name of the LHS type constructor (empty if the
    -- scanner couldn't identify it); it's stored alongside each
    -- collected constructor so downstream passes can tell which data
    -- decl a constructor belongs to.
    scanOneDataDecl !tyName !regs cur0 = do
        -- Peek at tokens to find either '=' (traditional) or 'where' (GADT).
        (eqOrWhere, curAfterSep) <- peekEqOrWhere cur0
        case eqOrWhere of
            TkWhere -> collectGadtCtors tyName 0 regs curAfterSep
            _       -> collectCtors     tyName 0 regs curAfterSep

    -- Scan forward until we find '=' (traditional) or 'where' (GADT).
    -- Returns the separator token kind and cursor after it.
    peekEqOrWhere cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEq    -> pure (TkEq, cur')
            TkWhere -> pure (TkWhere, cur')
            TkEof   -> pure (TkEof, cur)
            _       -> peekEqOrWhere cur'

    skipUntilEq :: Cursor -> IO Cursor
    skipUntilEq cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEq   -> pure cur'
            TkEof  -> pure cur
            _      -> skipUntilEq cur'

    -- Collect GADT-form constructors: each line is
    --   CtorName :: [ctx =>] T1 -> T2 -> ... -> ReturnType
    -- We count all '->' arrows at depth 0 and subtract 1 (last arrow
    -- leads to the return type, not a field). Plus one per constraint
    -- (each constraint in a tuple counts separately).
    --
    -- @tyName@ is the enclosing type-constructor name; @idx@ is the
    -- running 0-based declaration index assigned to each successfully
    -- parsed constructor (incremented as we collect them).
    collectGadtCtors !tyName !idx (!dReg, !fReg) cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkNewline -> collectGadtCtors tyName idx (dReg, fReg) cur'
            TkConId name -> do
                -- Expect '::' after constructor name.
                let (sep, curSig) = nextToken src cur'
                case tkKind sep of
                    TkDColon -> do
                        (arity, curEnd) <- countGadtArity 0 0 curSig
                        let dReg' = Map.insert name (tyName, arity, idx) dReg
                        collectGadtCtors tyName (idx + 1) (dReg', fReg) curEnd
                    _ -> collectGadtCtors tyName idx (dReg, fReg) cur'
            -- Column-1 non-newline means next top-level decl.
            _ | tkCol tok == 1 && tkKind tok /= TkNewline -> pure ((dReg, fReg), cur)
              | otherwise -> collectGadtCtors tyName idx (dReg, fReg) cur'

    -- Count arity of a GADT constructor signature.
    -- We count top-level '->' and ',' (for tuple constraints) minus 1.
    -- The '->' count minus 1 gives the number of fields (last one is return type).
    -- Each constraint before '=>' adds dictionary args.
    countGadtArity !arrows !dicts cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof -> pure (arrows + dicts, cur)
            TkNewline ->
                -- Each GADT ctor signature is on its own line.
                -- Stop at newline if the next token is col-1 (top-level)
                -- OR is a ConId (next GADT ctor in the where block).
                let (nxt, _) = nextToken src cur' in
                case tkKind nxt of
                    TkEof -> pure (arrows + dicts, cur')
                    TkConId _ -> pure (arrows + dicts, cur')
                    _ | tkCol nxt == 1 -> pure (arrows + dicts, cur')
                      | otherwise      -> countGadtArity arrows dicts cur'
            TkDArrow ->
                -- Everything before '=>' was constraints. Count commas as
                -- additional dicts (tuple constraints). Reset arrow count.
                countGadtArity 0 (dicts + max 1 arrows) cur'
            TkArrow -> countGadtArity (arrows + 1) dicts cur'
            TkLParen -> do
                -- Skip parenthesised types (tuples, etc.)
                curAfter <- skipToMatchingRParen 1 cur'
                countGadtArity arrows dicts curAfter
            _ -> countGadtArity arrows dicts cur'

    -- Skip forall binders up to and including the '.'.
    -- `forall a b c .` — skip everything until we see TkDot.
    skipForallBinders cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkDot -> pure cur'   -- '.' ends the forall binder list
            TkEof -> pure cur
            _     -> skipForallBinders cur'

    -- Peek ahead to decide if the current context is a constraint context.
    -- Returns True if we see '=>' before '|' or col-1 at depth 0.
    checkIfConstraint cur = go cur 0
      where
        go c depth = do
            let (tok, c') = nextToken src c
            case tkKind tok of
                TkEof    -> pure False
                TkDArrow | depth == 0 -> pure True
                TkBar    | depth == 0 -> pure False
                TkNewline ->
                    let (nxt, _) = nextToken src c' in
                    if tkCol nxt == 1
                        then pure False
                        else go c' depth
                TkLParen   -> go c' (depth + 1)
                TkRParen   -> go c' (max 0 (depth - 1))
                TkLBracket -> go c' (depth + 1)
                TkRBracket -> go c' (max 0 (depth - 1))
                _          -> go c' depth

    -- Skip everything up to and including '=>' at depth 0.
    skipConstraintContext cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof    -> pure cur
            TkDArrow -> pure cur'
            TkLParen   -> skipConstraintContext cur' >>= \c -> pure c
            _          -> skipConstraintContext cur'

    -- At start of each ctor: expect TkConId, then consume field atoms
    -- until TkBar / decl-end.
    -- Also handles existential prefix: `forall a. C a =>` before ctor name.
    --
    -- @tyName@ is the enclosing type-constructor name; @cIdx@ is the
    -- running 0-based declaration index of the *next* constructor to
    -- record. It's incremented only when a constructor is actually
    -- committed to the registry (constraint/existential prefixes don't
    -- advance it).
    collectCtors !tyName !cIdx (!dReg, !fReg) cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkNewline -> collectCtors tyName cIdx (dReg, fReg) cur'
            -- `forall tvars .` prefix — skip until '.' then check for constraints
            TkForall -> do
                curAfterDot <- skipForallBinders cur'
                collectCtors tyName cIdx (dReg, fReg) curAfterDot
            TkConId name -> do
                -- Check if this ConId is a class constraint (existential form).
                -- If we eventually hit '=>' before any '|' or col-1, it was a
                -- constraint context; skip it and restart collectCtors.
                isConstraint <- checkIfConstraint cur'
                if isConstraint
                    then do
                        -- Skip through the constraint(s) and '=>'.
                        curAfterArrow <- skipConstraintContext cur'
                        collectCtors tyName cIdx (dReg, fReg) curAfterArrow
                    else do
                        -- Peek ahead: if '{' follows immediately, it is record syntax.
                        let (peek, _) = nextToken src cur'
                        (arity, fields, curN) <- case tkKind peek of
                            TkLBrace -> do
                                let (_, curBrace) = nextToken src cur'
                                collectRecordFields 0 [] curBrace
                            _ -> do
                                (n, curN') <- countCtorFields 0 cur'
                                pure (n, [], curN')
                        let dReg' = Map.insert name (tyName, arity, cIdx) dReg
                            fReg' = foldr
                                (\(fieldName, idx) acc ->
                                    Map.insertWith (++) fieldName [(name, idx)] acc)
                                fReg
                                fields
                        -- After fields, check for '|' (more ctors) or decl end.
                        let (sep, curSep) = nextToken src curN
                        case tkKind sep of
                            TkBar     -> collectCtors tyName (cIdx + 1) (dReg', fReg') curSep
                            TkNewline -> collectCtors tyName (cIdx + 1) (dReg', fReg') curSep
                            _         -> pure ((dReg', fReg'), curN)
            -- Missing constructor (malformed) or decl ended.
            _ -> pure ((dReg, fReg), cur)

    -- Parse a record-syntax field list after the opening '{'.
    -- Returns (arity, [(fieldName, index)], cursorAfterClosingBrace).
    collectRecordFields !idx !fields cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof     -> pure (idx, reverse fields, cur)
            TkRBrace  -> pure (idx, reverse fields, cur')
            TkNewline -> collectRecordFields idx fields cur'
            TkComma   -> collectRecordFields idx fields cur'
            TkIdent fname -> do
                -- Skip '::' and the type expression up to next ',' or '}'.
                curAfterType <- skipFieldType cur'
                collectRecordFields (idx + 1) ((fname, idx) : fields) curAfterType
            TkBang -> do
                -- Strict field: '!' followed by identifier.
                let (fTok, cur'') = nextToken src cur'
                case tkKind fTok of
                    TkIdent fname -> do
                        curAfterType <- skipFieldType cur''
                        collectRecordFields (idx + 1) ((fname, idx) : fields) curAfterType
                    _ -> collectRecordFields idx fields cur''
            _ -> collectRecordFields idx fields cur'

    -- After a field name, skip '::' and the type annotation up to (but not
    -- consuming) the next ',' or '}'.
    skipFieldType cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkDColon -> skipType 0 cur'
            _        -> pure cur  -- no '::'; stop here

    -- Skip a type expression at the given depth.
    -- Stops (without consuming) at depth-0 ',' or '}'.
    skipType !depth cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof      -> pure cur
            TkComma    | depth == 0 -> pure cur   -- don't consume
            TkRBrace   | depth == 0 -> pure cur   -- don't consume
            TkLParen   -> skipType (depth + 1) cur'
            TkRParen   -> skipType (max 0 (depth - 1)) cur'
            TkLBracket -> skipType (depth + 1) cur'
            TkRBracket -> skipType (max 0 (depth - 1)) cur'
            TkLBrace   -> skipType (depth + 1) cur'
            TkRBrace   -> skipType (max 0 (depth - 1)) cur'
            TkNewline  -> do
                let (peek, _) = nextToken src cur'
                case tkKind peek of
                    TkEof -> pure cur'
                    _ | tkCol peek == 1 -> pure cur'
                      | otherwise       -> skipType depth cur'
            _          -> skipType depth cur'

    -- Count positional atoms up to '|', '{', column-1 token, or EOF.
    countCtorFields !n cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkBar            -> pure (n, cur)             -- don't consume
            TkEof            -> pure (n, cur)
            TkLBrace         -> pure (n, cur)             -- record block; stop
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
            TkPrimId _ -> countCtorFields (n + 1) cur'
            -- TkBang is a strictness annotation on the *next* field, not a
            -- field itself and not a terminator. Skip it and continue.
            TkBang    -> countCtorFields n cur'
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
-- Deriving Functor synthesis
--
-- A separate, lexer-only pass that walks every top-level @data@ or
-- @newtype@ declaration, captures the list of type variables, and for
-- each constructor classifies every positional/record field against the
-- LAST type variable (the Functor parameter). The result is consumed by
-- 'IHC.Scheduler.registerDerivedFunctorInstances' which synthesizes an
-- @fmap@ method Val per type and registers it in the class registry.
--
-- Grammar handled is a strict subset of 'scanDataDecls':
--   * Plain positional constructors: @C t1 t2 ...@
--   * Record-syntax constructors: @C { f1 :: T1, f2 :: T2 }@
-- (We skip GADT-form declarations for now; a @where@-style data decl
-- just won't get a derived Functor instance.)
--
-- A field's role is derived from its type token stream:
--   * If the type is exactly the functor tyvar ('TkIdent tvN'): FRVar
--   * Else if the type mentions the tyvar anywhere: FRRec
--   * Otherwise: FRNone
--------------------------------------------------------------------------------

-- | What to do with a constructor field when synthesizing @fmap@.
data FunctorFieldRole
    = FRNone   -- ^ Field type doesn't mention the functor tyvar; keep as-is.
    | FRVar    -- ^ Field type IS the functor tyvar; apply @f@ to the field.
    | FRRec    -- ^ Field type mentions the tyvar; apply @fmap f@ recursively.
    deriving (Eq, Show)

-- | Roles of every positional field in one derived-Functor constructor.
data FunctorCtor = FunctorCtor
    { fcName  :: !ByteString
    , fcRoles :: ![FunctorFieldRole]
    } deriving (Eq, Show)

-- | One @data T tv1 ... tvN = ... deriving (... Functor ...)@ decl with
-- the per-constructor field-role information needed to synthesize
-- @fmap@.
data FunctorDerivDecl = FunctorDerivDecl
    { fdTyName :: !ByteString
    , fdCtors  :: ![FunctorCtor]
    } deriving (Eq, Show)

-- | Scan the whole source for top-level @data@ / @newtype@ declarations
-- that carry @deriving Functor@ (in any position of the deriving list,
-- with or without a stock/newtype strategy, with or without
-- parentheses). For each such declaration, return a 'FunctorDerivDecl'
-- listing constructors and their per-field roles.
--
-- Declarations without @deriving Functor@ are silently skipped. If the
-- scanner can't determine the type's name or tyvars, the declaration is
-- also skipped (no entry in the result list).
scanFunctorDerivings :: Source -> IO [FunctorDerivDecl]
scanFunctorDerivings src = go [] startCursor
  where
    go !acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof -> pure (reverse acc)
            TkData    | tkCol tok == 1 -> handle acc cur'
            TkNewtype | tkCol tok == 1 -> handle acc cur'
            _ -> go acc cur'

    handle acc cur0 = do
        -- Parse header: optional '(ctx) =>' + type name + tyvars + '='
        mh <- parseDataHeader cur0
        case mh of
            Nothing -> go acc cur0
            Just (tyName, tyvars, curAfterEq) -> do
                (ctors, curAfterCtors) <- parseFunctorCtors tyvars curAfterEq
                derivClasses <- peekDerivingClause curAfterCtors
                if elem (BC.pack "Functor") derivClasses
                    then go (FunctorDerivDecl tyName ctors : acc) curAfterCtors
                    else go acc curAfterCtors

    -- Parse the LHS of a data/newtype decl up to the first '=' at depth 0.
    -- Returns (typeName, [tyvarName], cursorAfterEq) if it looks like a
    -- standard parameterised ADT, else Nothing.
    -- Skips an optional '(Ctx a) =>' prefix and any GADT '... where'.
    parseDataHeader :: Cursor -> IO (Maybe (ByteString, [ByteString], Cursor))
    parseDataHeader cur = do
        -- First, detect GADT-form by peeking for 'where' before '='.
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof    -> pure Nothing
            TkLParen -> do
                -- Could be '(Ctx a) =>' or '(,) a' (tuple head). Peek for
                -- '=>' to decide.
                let curAfter = skipParensPure 1 cur'
                    (sep, _) = nextToken src curAfter
                case tkKind sep of
                    TkDArrow ->
                        -- was a context; skip past '=>' and retry.
                        let (_, curAfterArr) = nextToken src curAfter
                        in parseDataHeader curAfterArr
                    _ -> pure Nothing   -- unusual LHS; skip
            TkConId name -> collectTyvars name [] cur'
            _ -> pure Nothing

    -- Collect tyvar names up to '=' (traditional) or give up at 'where'.
    collectTyvars tyName acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof    -> pure Nothing
            TkEq     -> pure (Just (tyName, reverse acc, cur'))
            TkWhere  -> pure Nothing       -- GADT; skip
            TkIdent v | isLowerStart v -> collectTyvars tyName (v : acc) cur'
            -- Kind annotation on a tyvar: '(a :: Type)' — strip.
            TkLParen -> do
                -- Grab the tyvar name inside the parens, then skip to ')'.
                let (inner, curIn) = nextToken src cur'
                case tkKind inner of
                    TkIdent v | isLowerStart v -> do
                        let curRest = skipParensPure 1 curIn
                        collectTyvars tyName (v : acc) curRest
                    _ -> collectTyvars tyName acc (skipParensPure 1 cur')
            TkNewline -> collectTyvars tyName acc cur'
            _ -> collectTyvars tyName acc cur'

    isLowerStart bs = case BC.uncons bs of
        Just (c, _) -> c >= 'a' && c <= 'z' || c == '_'
        Nothing     -> False

    -- Parse constructors (similar shape to collectCtors but also records
    -- each positional field's raw type token stream so we can classify
    -- against the Functor tyvar). Stops at the end of the data decl or
    -- at an explicit 'deriving' keyword. Returns (ctors, cursor-at-deriving-or-eof).
    parseFunctorCtors :: [ByteString] -> Cursor -> IO ([FunctorCtor], Cursor)
    parseFunctorCtors tyvars cur = loop [] cur
      where
        tvN = case reverse tyvars of
                (x : _) -> x
                []      -> BC.empty
        loop !acc c = do
            let (tok, c') = nextToken src c
            case tkKind tok of
                TkEof      -> pure (reverse acc, c)
                TkDeriving -> pure (reverse acc, c)       -- stop before consuming
                -- Top-level (col-1) anything except a newline → end of this decl.
                _ | tkCol tok == 1 && tkKind tok /= TkNewline ->
                      pure (reverse acc, c)
                TkNewline  -> loop acc c'
                TkForall   -> do
                    cAfterDot <- skipUntilDot c'
                    loop acc cAfterDot
                TkConId name -> do
                    -- Existential constraint context: skip if '=>' lies ahead
                    -- before a '|' at depth 0.
                    isCtx <- peekIsConstraint c'
                    if isCtx
                        then do
                            cAfterArr <- skipUntilDArrow c'
                            loop acc cAfterArr
                        else do
                            -- Peek: '{' → record syntax; else positional.
                            let (peek, _) = nextToken src c'
                            (roles, cEnd) <- case tkKind peek of
                                TkLBrace -> do
                                    let (_, cBrace) = nextToken src c'
                                    collectRecordRoles tvN cBrace
                                _ -> collectPositionalRoles tvN c'
                            let ctor = FunctorCtor name roles
                            -- After fields, see if another ctor follows.
                            let (sep, cSep) = nextToken src cEnd
                            case tkKind sep of
                                TkBar     -> loop (ctor : acc) cSep
                                _         -> pure (reverse (ctor : acc), cEnd)
                _ -> loop acc c'

    -- Positional field parser. Each field is a type-atom (single token or
    -- a parenthesised/bracketed group) optionally preceded by '!' or '~'.
    -- Stops at TkBar / TkDeriving / newline-col-1 / EOF.
    collectPositionalRoles :: ByteString -> Cursor -> IO ([FunctorFieldRole], Cursor)
    collectPositionalRoles tv cur = loop [] cur
      where
        loop !acc c = do
            let (tok, c') = nextToken src c
            case tkKind tok of
                TkEof      -> pure (reverse acc, c)
                TkBar      -> pure (reverse acc, c)
                TkDeriving -> pure (reverse acc, c)
                TkLBrace   -> pure (reverse acc, c)   -- record syntax boundary (unreachable here)
                TkNewline  ->
                    let (peek, _) = nextToken src c' in
                    case tkKind peek of
                        TkEof                        -> pure (reverse acc, c')
                        _ | tkCol peek == 1          -> pure (reverse acc, c')
                          | otherwise                -> loop acc c'
                TkBang -> loop acc c'                 -- strictness, skip
                -- Parenthesised group: read the whole balanced span as
                -- one field type atom.
                TkLParen -> do
                    (toks, cEnd) <- collectBalancedTokens TkLParen TkRParen c'
                    let role = roleOfTokens tv (TkLParen : toks ++ [TkRParen])
                    loop (role : acc) cEnd
                -- Bracketed group: @[T a]@.
                TkLBracket -> do
                    (toks, cEnd) <- collectBalancedTokens TkLBracket TkRBracket c'
                    let role = roleOfTokens tv (TkLBracket : toks ++ [TkRBracket])
                    loop (role : acc) cEnd
                TkConId n   -> loop (roleOfTokens tv [TkConId n] : acc) c'
                TkIdent n   -> loop (roleOfTokens tv [TkIdent n] : acc) c'
                TkPrimId n  -> loop (roleOfTokens tv [TkPrimId n] : acc) c'
                _ -> pure (reverse acc, c)

    -- Record-syntax field parser. Fields are @name1, name2, ... :: T1@
    -- groups separated by commas. For each named field we classify the
    -- type tokens (up to the next ',' or '}') and add one role per name.
    collectRecordRoles :: ByteString -> Cursor -> IO ([FunctorFieldRole], Cursor)
    collectRecordRoles tv cur = loop [] cur
      where
        loop !acc c = do
            let (tok, c') = nextToken src c
            case tkKind tok of
                TkEof    -> pure (reverse acc, c)
                TkRBrace -> pure (reverse acc, c')
                TkNewline -> loop acc c'
                TkComma  -> loop acc c'
                TkIdent _ -> do
                    -- Consume further names until '::'.
                    (nCount, cSig) <- consumeFieldNames 1 c'
                    -- Parse the type, capturing its tokens up to ',' or '}'.
                    (toks, cEnd) <- collectFieldTypeTokens cSig
                    let role = roleOfTokens tv toks
                    loop (replicate nCount role ++ acc) cEnd
                TkBang -> loop acc c'
                _ -> loop acc c'

        consumeFieldNames !n c = do
            let (tok, c') = nextToken src c
            case tkKind tok of
                TkComma -> do
                    -- Peek — if next is another ident, that's another name
                    -- sharing the same type signature.
                    let (nxt, cNxt) = nextToken src c'
                    case tkKind nxt of
                        TkIdent _ -> consumeFieldNames (n + 1) cNxt
                        _         -> pure (n, c)           -- shouldn't happen in record syntax
                TkDColon -> pure (n, c')
                TkBang   -> consumeFieldNames n c'
                TkIdent _ -> consumeFieldNames (n + 1) c'  -- @a, b :: T@
                _        -> pure (n, c)

        collectFieldTypeTokens = collectUntilCommaOrBrace []
        collectUntilCommaOrBrace !acc c = do
            let (tok, c') = nextToken src c
            case tkKind tok of
                TkEof    -> pure (reverse acc, c)
                TkComma  -> pure (reverse acc, c)
                TkRBrace -> pure (reverse acc, c)
                TkLParen -> do
                    (inner, cEnd) <- collectBalancedTokens TkLParen TkRParen c'
                    collectUntilCommaOrBrace (reverse (TkLParen : inner ++ [TkRParen]) ++ acc) cEnd
                TkLBracket -> do
                    (inner, cEnd) <- collectBalancedTokens TkLBracket TkRBracket c'
                    collectUntilCommaOrBrace (reverse (TkLBracket : inner ++ [TkRBracket]) ++ acc) cEnd
                TkNewline -> collectUntilCommaOrBrace acc c'
                _ -> collectUntilCommaOrBrace (tkKind tok : acc) c'

    -- Collect tokens between balanced open/close brackets, consuming the
    -- closing token. Returns (inner tokens, cursor after close).
    collectBalancedTokens :: TokenKind -> TokenKind -> Cursor -> IO ([TokenKind], Cursor)
    collectBalancedTokens open close cur = go' 1 [] cur
      where
        go' !depth !acc c
            | depth <= 0 = pure (reverse acc, c)
            | otherwise = do
                let (tok, c') = nextToken src c
                case tkKind tok of
                    TkEof -> pure (reverse acc, c')
                    k | k == close ->
                            if depth - 1 <= 0
                                then pure (reverse acc, c')
                                else go' (depth - 1) (k : acc) c'
                      | k == open  -> go' (depth + 1) (k : acc) c'
                      | otherwise  -> go' depth (k : acc) c'

    -- Classify a field's type-token stream against the functor tyvar.
    -- * [TkIdent tv]                           → FRVar
    -- * tokens mention TkIdent tv elsewhere    → FRRec
    -- * otherwise                              → FRNone
    roleOfTokens :: ByteString -> [TokenKind] -> FunctorFieldRole
    roleOfTokens tv toks
        | BC.null tv = FRNone
        | toks == [TkIdent tv] = FRVar
        | any (== TkIdent tv) toks = FRRec
        | otherwise = FRNone

    -- Peek at the cursor (positioned right AFTER the data-decl body has
    -- ended) for a 'deriving' clause, and return the list of class names
    -- it mentions (empty list if there is no clause). Handles the shapes:
    --
    -- @
    -- deriving Cls
    -- deriving (Cls1, Cls2, ...)
    -- deriving stock/newtype/anyclass (Cls, ...)
    -- deriving (Cls) via ...
    -- @
    peekDerivingClause :: Cursor -> IO [ByteString]
    peekDerivingClause cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkDeriving -> scanClasses cur'
            TkNewline  -> peekDerivingClause cur'
            _          -> pure []

    scanClasses cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof    -> pure []
            -- Optional strategy keyword (stock / newtype / anyclass): the
            -- Haskell lexer sees these as TkIdent.
            TkIdent s | s == BC.pack "stock"
                     || s == BC.pack "anyclass" -> scanClasses cur'
            TkNewtype  -> scanClasses cur'
            TkLParen  -> collectClassList [] cur'
            TkConId c -> pure [c]
            _         -> pure []

    collectClassList !acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof     -> pure (reverse acc)
            TkRParen  -> pure (reverse acc)
            TkConId c -> collectClassList (c : acc) cur'
            _         -> collectClassList acc cur'

    -- Local helpers — smaller / specialised versions of the ones inside
    -- 'scanDataDecls' that only advance the cursor.
    skipUntilDot cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof -> pure cur
            TkDot -> pure cur'
            _     -> skipUntilDot cur'

    skipUntilDArrow cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof    -> pure cur
            TkDArrow -> pure cur'
            _        -> skipUntilDArrow cur'

    peekIsConstraint cur = go' cur 0
      where
        go' c !depth = do
            let (tok, c') = nextToken src c
            case tkKind tok of
                TkEof -> pure False
                TkDArrow | depth == 0 -> pure True
                TkBar    | depth == 0 -> pure False
                TkEq     | depth == 0 -> pure False
                TkNewline ->
                    let (nxt, _) = nextToken src c' in
                    if tkCol nxt == 1
                        then pure False
                        else go' c' depth
                TkLParen -> go' c' (depth + 1)
                TkRParen -> go' c' (max 0 (depth - 1))
                TkLBracket -> go' c' (depth + 1)
                TkRBracket -> go' c' (max 0 (depth - 1))
                _ -> go' c' depth

    skipParensPure :: Int -> Cursor -> Cursor
    skipParensPure d c
        | d <= 0 = c
        | otherwise =
            let (tk, c') = nextToken src c
            in case tkKind tk of
                TkEof    -> c
                TkLParen -> skipParensPure (d + 1) c'
                TkRParen -> skipParensPure (d - 1) c'
                _        -> skipParensPure d c'

--------------------------------------------------------------------------------
-- Instance declarations
--------------------------------------------------------------------------------

-- | One top-level @instance C T where method = body@ declaration.
-- 'instClassName' is the class name, 'instTypeName' is the head type
-- (first uppercase identifier after the class name, before @where@ — or
-- for MPTC heads, the first parameter; see 'instTypeNames' for the full
-- list). 'instMethods' is a list of (method-name, Clause-list) pairs
-- exactly like the output of 'findBinding'.
--
-- 'instTypeNames' holds every head-position type argument in source
-- order. For a single-parameter class this is a one-element list
-- @[instTypeName]@. For a multi-parameter class like
-- @instance SetField \"name\" User String where ...@ it is
-- @[\"name\", \"User\", \"String\"]@ (normalised via
-- 'IHC.Classes.normalizeTyTag' — quotes stripped).
data InstanceDecl = InstanceDecl
    { instClassName :: !ByteString
    , instTypeName  :: !ByteString
    , instTypeNames :: ![ByteString]
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

    -- After `instance`, scan the head to find class name + type names,
    -- then scan the `where` body for method bindings.
    scanOneInstance cur0 = do
        -- Collect tokens up to `where`.
        (mClassName, tyArgs, curWhere) <- parseInstanceHead cur0
        case (mClassName, tyArgs) of
            (Just cls, (firstTyp : _)) -> do
                methods <- parseInstanceBody curWhere
                pure (Just (InstanceDecl cls firstTyp tyArgs methods))
            _ -> pure Nothing

    -- Parse: [context =>] ClassName TyHead1 TyHead2 ... where
    -- We grab the first TkConId (after any constraint @=>@) as class
    -- name, then every subsequent head token becomes a type argument.
    -- 'TkConId' / 'TkStr' / 'TkInt' / 'TkIdent' at the top level produce
    -- their raw bytes; parenthesised groups are kept verbatim so the
    -- caller can pattern-match them if needed.  Each tag is normalised
    -- via 'IHC.Classes.normalizeTyTag' so registration and dispatch
    -- agree on the key.
    parseInstanceHead cur0 = scanHead cur0 Nothing [] False
      where
        scanHead cur mCls acc seenArrow = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkEof    -> pure (mCls, reverse acc, cur)
                TkWhere  -> pure (mCls, reverse acc, cur')
                TkNewline -> scanHead cur' mCls acc seenArrow
                TkDArrow -> scanHead cur' mCls [] True  -- reset acc past context
                TkConId n ->
                    case mCls of
                        Nothing -> scanHead cur' (Just n) acc seenArrow
                        Just _  -> scanHead cur' mCls (normalize n : acc) seenArrow
                TkStr s | isJust mCls ->
                    -- DataKinds Symbol literal used as a type arg, e.g.
                    -- @instance SetField "name" ...@. Store the string
                    -- contents verbatim (quotes already stripped by the
                    -- lexer — 'TkStr' holds the decoded bytes).
                    scanHead cur' mCls (normalize s : acc) seenArrow
                TkInt i | isJust mCls ->
                    -- DataKinds Nat literal used as a type arg.
                    scanHead cur' mCls (BC.pack (show i) : acc) seenArrow
                TkChar c | isJust mCls ->
                    -- DataKinds Char literal used as a type arg.
                    scanHead cur' mCls (BC.pack [c] : acc) seenArrow
                TkIdent n | isJust mCls ->
                    -- Lower-case identifier used as a type arg (rare —
                    -- typically a type variable in the context). Capture
                    -- it so the tag list has a sensible length; callers
                    -- use this for composite-key registration.
                    scanHead cur' mCls (normalize n : acc) seenArrow
                TkIdent _ ->
                    -- Lower-case type variable before the class name;
                    -- skip.
                    scanHead cur' mCls acc seenArrow
                TkLParen -> do
                    curAfter <- skipParens 1 cur'
                    let acc' = case mCls of
                            Nothing -> acc  -- still pre-class; ignore
                            Just _  -> BC.pack "(,)" : acc
                    scanHead curAfter mCls acc' seenArrow
                TkLBracket -> do
                    curAfter <- skipBrackets 1 cur'
                    let acc' = case mCls of
                            Nothing -> acc
                            Just _  -> BC.pack "[]" : acc
                    scanHead curAfter mCls acc' seenArrow
                _ -> scanHead cur' mCls acc seenArrow

        isJust (Just _) = True
        isJust Nothing  = False

        normalize = normalizeTyTag

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
                -- InstanceSigs: if this line is a type sig
                -- (@name[, name]* :: type@), skip it silently. Otherwise
                -- fall through to the normal clause-scanning path.
                mSkip <- trySkipInstanceSig (tkCol tok) cur'
                case mSkip of
                    Just curAfter -> scanMethods acc curAfter
                    Nothing -> do
                        mClause <- scanOneClauseAfterNameAtCol src (tkCol tok) cur'
                        case mClause of
                            Nothing -> scanMethods acc cur'
                            Just (clause, curNext) -> do
                                (moreClauses, curFinal) <-
                                    collectInstanceClauses name (tkCol tok) [clause] curNext
                                let lhs  = BindingLhs (reverse moreClauses)
                                    acc' = Map.insert name lhs acc
                                scanMethods acc' curFinal
            -- Phase 3.6: operator methods like @(>>=)@, @(>>)@, @(<>)@, @(+)@.
            -- Pattern: @TkLParen TkSymOp name TkRParen ...@
            TkLParen | tkCol tok > 1 -> do
                let (opTok, cur'') = nextToken src cur'
                case tkKind opTok of
                    TkSymOp opName -> do
                        let (closeTok, cur''') = nextToken src cur''
                        case tkKind closeTok of
                            TkRParen -> do
                                -- InstanceSigs: @(<>) :: ...@ inside an
                                -- instance body is a type sig, not a
                                -- binding. Detect and skip.
                                let (peek, _) = peekSig cur'''
                                case tkKind peek of
                                    TkDColon -> do
                                        curAfter <- skipInstanceSigType (tkCol tok) cur'''
                                        scanMethods acc curAfter
                                    _ -> do
                                        mClause <- scanOneClauseAfterNameAtCol src (tkCol tok) cur'''
                                        case mClause of
                                            Nothing -> scanMethods acc cur'
                                            Just (clause, curNext) -> do
                                                let lhs  = BindingLhs [clause]
                                                    acc' = Map.insert opName lhs acc
                                                scanMethods acc' curNext
                            _ -> scanMethods acc cur'
                    _ -> scanMethods acc cur'
            -- Phase 3.2: skip associated-type declarations inside instance bodies.
            -- 'type Elem [] = ()' appears inside 'instance C T where' blocks.
            -- We use findLineEnd to skip only the current line so we don't
            -- accidentally consume sibling method bindings at the same indent.
            TkTypeKw | tkCol tok > 1 ->
                let lineEnd = findLineEnd src (cPos cur')
                    curNext = Cursor lineEnd 0 (tkCol tok)
                in scanMethods acc curNext
            -- Stop when we hit a new column-1 token (next top-level decl).
            _ | tkCol tok == 1 && tkKind tok /= TkNewline -> pure (Map.toList acc)
              | otherwise -> scanMethods acc cur'

    collectInstanceClauses name bindCol acc cur = do
        let (tok, curAfter) = peekSig cur
        case tkKind tok of
            TkIdent n | n == name && tkCol tok == bindCol -> do
                mClause <- scanOneClauseAfterNameAtCol src bindCol curAfter
                case mClause of
                    Nothing -> pure (acc, cur)
                    Just (cl, curNext) ->
                        collectInstanceClauses name bindCol (cl : acc) curNext
            _ -> pure (acc, cur)

    peekSig cur0 =
        let (tok, curN) = nextToken src cur0 in
        case tkKind tok of
            TkNewline -> peekSig curN
            _         -> (tok, curN)

    -- | InstanceSigs: try to detect and skip a type signature line of the
    -- form @name[, name]* :: type@ at the start of an instance-body
    -- clause. The caller has already consumed the first @name@ at
    -- column @bindCol@; @cur@ is positioned just after that identifier.
    --
    -- Returns @Just curAfter@ if a sig was skipped (the cursor is at
    -- the next token with column @<= bindCol@, i.e. the next method or
    -- the end of the instance body). Returns @Nothing@ if this looks
    -- like a normal value binding and the caller should handle it.
    trySkipInstanceSig bindCol cur = do
        let (t, c) = peekSig cur
        case tkKind t of
            TkDColon -> do
                curAfter <- skipInstanceSigType bindCol c
                pure (Just curAfter)
            TkComma -> do
                -- Multi-name sig: consume comma, expect another ident, recurse.
                let (t2, c2) = peekSig c
                case tkKind t2 of
                    TkIdent _ -> trySkipInstanceSig bindCol c2
                    -- Qualified/operator variants unlikely inside an
                    -- instance sig — if we can't continue, fall through.
                    _ -> pure Nothing
            _ -> pure Nothing

    -- | Skip the type body of a @name :: <type>@ signature inside an
    -- instance (or class) body. Stops as soon as a significant token
    -- appears at column @<= bindCol@ at bracket depth 0, which marks
    -- the next method binding or the end of the block.
    skipInstanceSigType bindCol cur0 = go cur0 (0 :: Int) (0 :: Int) (0 :: Int)
      where
        go cur b p c = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkEof     -> pure cur
                TkNewline -> go cur' b p c
                _ | tkCol tok <= bindCol && b == 0 && p == 0 && c == 0 -> pure cur
                TkLParen   -> go cur' b (p + 1) c
                TkLBracket -> go cur' (b + 1) p c
                TkLBrace   -> go cur' b p (c + 1)
                TkRParen   | p > 0 -> go cur' b (p - 1) c
                TkRParen           -> pure cur
                TkRBracket | b > 0 -> go cur' (b - 1) p c
                TkRBracket         -> pure cur
                TkRBrace   | c > 0 -> go cur' b p (c - 1)
                TkRBrace           -> pure cur
                _          -> go cur' b p c

--------------------------------------------------------------------------------
-- Class declarations
--------------------------------------------------------------------------------

-- | One top-level @class C a where ...@ declaration.
--
-- 'classClassName' is the class name (first uppercase identifier after
-- @class@, ignoring any optional @(ctx) =>@ superclass context).
--
-- 'classMethodNames' is the alphabetical list of every declared method
-- name. The order matches what 'scanInstanceDecls' produces for instance
-- bodies so that positional slot dispatch lines up.
--
-- 'classDefaults' is a Map from method-name to a 'BindingLhs' capturing
-- a default-method body (if one is provided in the class body). Only
-- methods with a default appear in the map.
data ClassDecl = ClassDecl
    { classClassName   :: !ByteString
    , classMethodNames :: ![ByteString]
    , classDefaults    :: !(Map ByteString BindingLhs)
    } deriving (Eq, Show)

-- | Scan the whole source for top-level @class@ declarations and return
-- each one as a 'ClassDecl'. We mirror 'scanInstanceDecls': the class
-- head is parsed loosely (we grab the first ConId after any optional
-- context), the @where@-body is walked for both method-signature lines
-- (@name[, name]* :: type@) and default-method bindings
-- (@name pat* = expr@). Type sigs contribute to the method-name set;
-- default bodies are captured as 'BindingLhs' byte-spans for later parse.
--
-- Method names are returned in alphabetical order so positional slot
-- dispatch matches 'scanInstanceDecls' (which uses @Map.toList@).
scanClassDecls :: Source -> IO [ClassDecl]
scanClassDecls src = go [] startCursor
  where
    go !acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof -> pure (reverse acc)
            TkClass | tkCol tok == 1 -> do
                mDecl <- scanOneClass cur'
                case mDecl of
                    Nothing   -> go acc cur'
                    Just decl -> go (decl : acc) cur'
            _ -> go acc cur'

    -- After `class`, parse head to find the class name, then walk the
    -- `where` body collecting method sigs + default-method bindings.
    scanOneClass cur0 = do
        (mClassName, curWhere) <- parseClassHead cur0
        case mClassName of
            Just cls -> do
                (methodNames, defaults) <- parseClassBody curWhere
                pure (Just (ClassDecl cls methodNames defaults))
            Nothing -> pure Nothing

    -- Parse: [context =>] ClassName tyvar* [| fundep, ...] where
    -- Grab the first ConId AFTER any =>.  Stop at `where`.
    --
    -- FunctionalDependencies: the optional `| a b -> c, d -> e` clause
    -- between the class-head params and `where` has no runtime meaning
    -- in IHC (fundeps are a type-checker hint in GHC), so on `TkBar`
    -- we consume tokens until `where` without touching mCls. This keeps
    -- lowercase tyvars in the fundep body from ever being mistaken for
    -- something meaningful, and documents the intent explicitly rather
    -- than relying on the default fallthrough.
    parseClassHead cur0 = scanHead cur0 Nothing False
      where
        scanHead cur mCls seenArrow = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkEof    -> pure (mCls, cur)
                TkWhere  -> pure (mCls, cur')
                TkNewline -> scanHead cur' mCls seenArrow
                -- `=>` ends the context; reset our candidate class name
                -- so the NEXT ConId is the real class.
                TkDArrow -> scanHead cur' Nothing True
                -- FunctionalDependencies: `| a -> b, c -> d` — skip the
                -- whole fundep clause, stopping only at `where`/EOF.
                TkBar -> skipFundep cur' mCls
                TkConId n ->
                    case mCls of
                        Nothing -> scanHead cur' (Just n) seenArrow
                        Just _  -> scanHead cur' mCls seenArrow
                TkLParen -> do
                    curAfter <- skipParensC 1 cur'
                    scanHead curAfter mCls seenArrow
                TkLBracket -> do
                    curAfter <- skipBracketsC 1 cur'
                    scanHead curAfter mCls seenArrow
                _ -> scanHead cur' mCls seenArrow

        -- After `|` in the class head, consume the fundep list without
        -- interpreting it. Stop at `where`/EOF; keep any class-name
        -- candidate already captured in @mCls@.
        skipFundep cur mCls = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkEof   -> pure (mCls, cur)
                TkWhere -> pure (mCls, cur')
                TkLParen -> do
                    curAfter <- skipParensC 1 cur'
                    skipFundep curAfter mCls
                TkLBracket -> do
                    curAfter <- skipBracketsC 1 cur'
                    skipFundep curAfter mCls
                _ -> skipFundep cur' mCls

    skipParensC :: Int -> Cursor -> IO Cursor
    skipParensC !d cur
        | d <= 0    = pure cur
        | otherwise = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkLParen -> skipParensC (d + 1) cur'
                TkRParen -> skipParensC (d - 1) cur'
                TkEof    -> pure cur'
                _        -> skipParensC d cur'

    skipBracketsC :: Int -> Cursor -> IO Cursor
    skipBracketsC !d cur
        | d <= 0    = pure cur
        | otherwise = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkLBracket -> skipBracketsC (d + 1) cur'
                TkRBracket -> skipBracketsC (d - 1) cur'
                TkEof      -> pure cur'
                _          -> skipBracketsC d cur'

    -- Parse the `where` body of a class. Each binding is either:
    --   * a type sig: `name[, name]* :: type`   — adds names to method set
    --   * a default:  `name [pat*] = body`      — captured as BindingLhs
    parseClassBody cur0 = scanBody Map.empty Map.empty cur0
      where
        -- @sigs@: Map from method-name to () (acts as a Set).
        -- @defs@: Map from method-name to default BindingLhs.
        scanBody !sigs !defs cur = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkEof     -> pure (finishBody sigs defs)
                TkNewline -> scanBody sigs defs cur'
                TkIdent name | tkCol tok > 1 -> do
                    -- Check whether this line is a type sig. If so, collect
                    -- every name before `::` and skip through the type.
                    mSigNames <- trySkipClassSig (tkCol tok) name cur'
                    case mSigNames of
                        Just (names, curAfter) -> do
                            let sigs' = foldr (`Map.insert` ()) sigs names
                            scanBody sigs' defs curAfter
                        Nothing -> do
                            -- Default-method body: `name pat* = rhs` (or guards).
                            mClause <- scanOneClauseAfterNameAtCol src (tkCol tok) cur'
                            case mClause of
                                Nothing -> scanBody sigs defs cur'
                                Just (clause, curNext) -> do
                                    (moreClauses, curFinal) <-
                                        collectClassClauses name (tkCol tok) [clause] curNext
                                    let lhs  = BindingLhs (reverse moreClauses)
                                        defs' = Map.insert name lhs defs
                                    scanBody sigs defs' curFinal
                -- Operator methods: `(<>) :: ...` or `(<>) x y = ...`
                TkLParen | tkCol tok > 1 -> do
                    let (opTok, cur'') = nextToken src cur'
                    case tkKind opTok of
                        TkSymOp opName -> do
                            let (closeTok, cur''') = nextToken src cur''
                            case tkKind closeTok of
                                TkRParen -> do
                                    -- Peek: `::` means type sig, otherwise default.
                                    let (peek, _) = peekSigC cur'''
                                    case tkKind peek of
                                        TkDColon -> do
                                            mNames <- trySkipClassSig (tkCol tok) opName cur'''
                                            case mNames of
                                                Just (names, curAfter) -> do
                                                    let sigs' = foldr (`Map.insert` ()) sigs names
                                                    scanBody sigs' defs curAfter
                                                Nothing -> scanBody sigs defs cur'
                                        _ -> do
                                            mClause <- scanOneClauseAfterNameAtCol src (tkCol tok) cur'''
                                            case mClause of
                                                Nothing -> scanBody sigs defs cur'
                                                Just (clause, curNext) -> do
                                                    let lhs  = BindingLhs [clause]
                                                        defs' = Map.insert opName lhs defs
                                                    scanBody sigs defs' curNext
                                _ -> scanBody sigs defs cur'
                        _ -> scanBody sigs defs cur'
                -- Associated-type declarations inside class bodies: skip.
                TkTypeKw | tkCol tok > 1 ->
                    let lineEnd = findLineEnd src (cPos cur')
                        curNext = Cursor lineEnd 0 (tkCol tok)
                    in scanBody sigs defs curNext
                -- Hit a new column-1 token: end of class body.
                _ | tkCol tok == 1 && tkKind tok /= TkNewline ->
                      pure (finishBody sigs defs)
                  | otherwise -> scanBody sigs defs cur'

        finishBody sigs defs =
            -- Method-names = union of sigs and default-bodies, sorted.
            let allNames = Map.keys (Map.union sigs (Map.map (const ()) defs))
            in (allNames, defs)

    -- | Try to parse a @name[, name]* :: type@ signature line inside the
    -- class body. @firstName@ is the identifier at the start of the line
    -- (already consumed by the caller). @cur@ is positioned just after
    -- that identifier. Returns @Just (names, curAfter)@ if the line is a
    -- signature.
    trySkipClassSig bindCol firstName cur = do
        let (t, c) = peekSigC cur
        case tkKind t of
            TkDColon -> do
                curAfter <- skipClassSigType bindCol c
                pure (Just ([firstName], curAfter))
            TkComma -> do
                -- Walk a comma-separated list of names.
                (names, curAfter) <- collectSigNames [firstName] c
                case names of
                    [] -> pure Nothing
                    _  -> pure (Just (names, curAfter))
            _ -> pure Nothing

    collectSigNames acc cur = do
        let (t, c) = peekSigC cur
        case tkKind t of
            TkIdent n -> do
                let (t2, c2) = peekSigC c
                case tkKind t2 of
                    TkComma  -> collectSigNames (n : acc) c2
                    TkDColon -> do
                        curAfter <- skipClassSigType 0 c2
                        pure (reverse (n : acc), curAfter)
                    _ -> pure ([], cur)
            TkLParen -> do
                -- Operator name in sig list: (<>).
                let (opTok, c2) = peekSigC c
                case tkKind opTok of
                    TkSymOp opName -> do
                        let (t3, c3) = peekSigC c2
                        case tkKind t3 of
                            TkRParen -> do
                                let (t4, c4) = peekSigC c3
                                case tkKind t4 of
                                    TkComma  -> collectSigNames (opName : acc) c4
                                    TkDColon -> do
                                        curAfter <- skipClassSigType 0 c4
                                        pure (reverse (opName : acc), curAfter)
                                    _ -> pure ([], cur)
                            _ -> pure ([], cur)
                    _ -> pure ([], cur)
            _ -> pure ([], cur)

    -- Skip the type body after a `::`.  Stops when a significant token
    -- appears at column @<= bindCol@ at bracket depth 0.
    skipClassSigType bindCol cur0 = go cur0 (0 :: Int) (0 :: Int) (0 :: Int)
      where
        go cur b p c = do
            let (tok, cur') = nextToken src cur
            case tkKind tok of
                TkEof     -> pure cur
                TkNewline -> go cur' b p c
                _ | tkCol tok <= bindCol && b == 0 && p == 0 && c == 0 -> pure cur
                TkLParen   -> go cur' b (p + 1) c
                TkLBracket -> go cur' (b + 1) p c
                TkLBrace   -> go cur' b p (c + 1)
                TkRParen   | p > 0 -> go cur' b (p - 1) c
                TkRParen           -> pure cur
                TkRBracket | b > 0 -> go cur' (b - 1) p c
                TkRBracket         -> pure cur
                TkRBrace   | c > 0 -> go cur' b p (c - 1)
                TkRBrace           -> pure cur
                _          -> go cur' b p c

    -- Continuation clauses of a default-method body.
    collectClassClauses name bindCol acc cur = do
        let (tok, curAfter) = peekSigC cur
        case tkKind tok of
            TkIdent n | n == name && tkCol tok == bindCol -> do
                mClause <- scanOneClauseAfterNameAtCol src bindCol curAfter
                case mClause of
                    Nothing -> pure (acc, cur)
                    Just (cl, curNext) ->
                        collectClassClauses name bindCol (cl : acc) curNext
            _ -> pure (acc, cur)

    peekSigC cur0 =
        let (tok, curN) = nextToken src cur0 in
        case tkKind tok of
            TkNewline -> peekSigC curN
            _         -> (tok, curN)
