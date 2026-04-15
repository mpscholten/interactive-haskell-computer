{-# LANGUAGE DeriveAnyClass #-}

-- | Single-pass recursive-descent parser producing AST.
--
-- Phase 2.2.5: supports multi-clause function definitions and pattern
-- guards on the LHS.
--
--   * An 'IHC.Scan.BindingLhs' now carries a LIST of 'Clause's; each
--     clause is a (pattern-span, rhs-span) pair of byte ranges.
--   * 'parseBodyExpr' parses every clause's patterns, parses its RHS
--     (either @= expr@ or @| g1 = e1 | g2 = e2 ...@), then desugars
--     the whole group into a lambda chain over case-expressions with
--     fall-through to the next clause.
--   * A single-clause binding whose patterns are all 'PVar'/'PWild'
--     collapses to the classic @ELam x body@ chain (preserves laziness
--     for plain @f x = ...@ definitions).
--
-- All parsers respect a body-end @Pos@ (carried in 'Ctx') so the
-- expression grammar — which freely spans newlines — does NOT
-- swallow the next top-level binding's tokens. nextSig synthesises
-- a virtual @TkEof@ when the cursor reaches that bound.
module IHC.Parser
    ( parseBodyExpr
    , ParseError(..)
    ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import IHC.AST
import IHC.Lexer
import IHC.Scan (Clause(..))
import IHC.Source

newtype ParseError = ParseError String
    deriving stock (Show)
    deriving anyclass (Exception)

-- | Parser context bundle.
data Ctx = Ctx
    { ctxSrc    :: !Source
    , ctxEnd    :: !Pos      -- byte-end of the region we're parsing
    , ctxMinCol :: !Int      -- the layout column: an inner expression may
                             --   freely consume tokens at column > ctxMinCol
                             --   (continuation), but a token at column
                             --   <= ctxMinCol is the start of a new layout
                             --   item (or end of block) and must NOT be
                             --   absorbed by the inner parsers.
    }

-- | Parse all clauses of a top-level binding, returning a single
-- 'Expr' that desugars multi-clause function definitions (including
-- pattern guards) into a lambda chain + case fall-through structure.
--
-- For the degenerate one-clause all-var-pattern case — e.g. @f x y = e@
-- — the result is the classic @ELam x (ELam y e)@ (no case wrapping),
-- preserving the laziness guarantees from earlier phases.
parseBodyExpr :: Source -> [Clause] -> IO Expr
parseBodyExpr _   [] = throwIO (ParseError "parseBodyExpr: empty clause list")
parseBodyExpr src clauses = do
    parsed <- mapM (parseClause src) clauses
    let arity = case parsed of
            ((ps, _) : _) -> length ps
            _             -> 0
    -- Sanity: every clause must have the same number of patterns.
    mapM_ (\(ps, _) ->
                if length ps == arity
                    then pure ()
                    else throwIO (ParseError
                        "parseBodyExpr: clauses have differing arities"))
          parsed
    pure (desugarClauses parsed arity)

--------------------------------------------------------------------------------
-- Parsed clauses
--------------------------------------------------------------------------------

-- | A parsed clause: its patterns and its RHS (a plain body or a list
-- of guarded branches). The body of each guard is already fully
-- parsed (with its own where-clause applied if present).
type ParsedClause = ([Pat], Rhs)

data Rhs
    = RhsPlain  !Expr
    | RhsGuards ![(Expr, Expr)]   -- each (guard, body); tried in order

-- | Parse a single clause: its patterns (byte span) and its RHS
-- (byte span starting with either @=@ or @|@).
parseClause :: Source -> Clause -> IO ParsedClause
parseClause src (Clause patsSpan rhsSpan) = do
    -- A clause-RHS may contain a trailing where-clause shared by all
    -- the guard bodies (for guarded) or the single body (for plain).
    let (coreSpan, mWhere) = splitOnWhere src rhsSpan
    whereBinds <- case mWhere of
        Nothing -> pure []
        Just ws -> parseBindingsIn src ws

    pats <- parsePatsIn src patsSpan
    rhs  <- parseRhsIn src coreSpan
    -- Attach where-clause to every guarded body / the plain body.
    let wrapWhere e = case whereBinds of
            [] -> e
            bs -> ELet bs e
    let rhs' = case rhs of
            RhsPlain e    -> RhsPlain  (wrapWhere e)
            RhsGuards ges -> RhsGuards [(g, wrapWhere b) | (g, b) <- ges]
    pure (pats, rhs')

--------------------------------------------------------------------------------
-- LHS pattern parsing
--------------------------------------------------------------------------------

-- | Parse the sequence of parameter patterns inside a byte span. The
-- span covers the bytes between the binder-name and the first @=@/@|@.
-- The span may be empty (no params).
parsePatsIn :: Source -> Span -> IO [Pat]
parsePatsIn src (start, end) = do
    let ctx  = Ctx src end 0
        cur0 = Cursor start 1 1
    loop ctx cur0 []
  where
    loop ctx cur acc = do
        let (tok, _) = nextSig ctx cur
        -- Stop at EOF (span exhausted) or at any token that can't
        -- start a pattern (e.g. the `=`/`|` that delimits the RHS —
        -- this token sits just at or past the span's end).
        if tkKind tok == TkEof || not (startsPat (tkKind tok))
            then pure (reverse acc)
            else do
                (p, cur') <- parseSubPat ctx cur
                loop ctx cur' (p : acc)

-- | Parse the RHS of a clause, given a span whose first token is
-- either @=@ (plain) or @|@ (guarded). For guarded RHSs we gather
-- every @| guard = body@ branch until the span ends.
parseRhsIn :: Source -> Span -> IO Rhs
parseRhsIn src (start, end) = do
    let ctx  = Ctx src end 1
        cur0 = Cursor start 1 1
        (firstTok, cur1) = nextSig ctx cur0
    case tkKind firstTok of
        TkEq -> do
            (e, _) <- parseExpr ctx cur1
            pure (RhsPlain e)
        TkBar -> do
            -- Parse one guard branch, then loop for more bars.
            branches <- parseGuards ctx cur1 []
            pure (RhsGuards branches)
        _ -> parseErr "expected `=` or `|` at start of RHS" firstTok
  where
    -- At cur, we're just past a `|`. Parse <guard> `=` <body>, then
    -- if the next significant token is `|` continue.
    parseGuards ctx cur acc = do
        (g, cur1) <- parseExpr ctx cur
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr "expected `=` after guard" eqTok
        (b, cur3) <- parseExpr ctx cur2
        let (sep, cur4) = nextSig ctx cur3
        case tkKind sep of
            TkBar -> parseGuards ctx cur4 ((g, b) : acc)
            _     -> pure (reverse ((g, b) : acc))

--------------------------------------------------------------------------------
-- Clause desugaring
--------------------------------------------------------------------------------

-- | Turn a list of parsed clauses into a single 'Expr'. Algorithm:
--
--   1. If it's one clause with all-var/wild patterns and a plain body,
--      emit @λp1. … λpn. body@ (classic, laziness-preserving).
--
--   2. Otherwise, bind fresh arg names @$a0 … $a(n-1)@ and, for each
--      clause, generate nested @case@s that try to match each pattern
--      position against its corresponding arg. Failures fall through
--      to the next clause's attempt, tracked via a 'let'-bound name
--      so the fallback isn't duplicated across every nested case.
--
-- Guards are compiled to a nested if-chain inside the successful match
-- body; if every guard fails, the clause falls through to the next.
desugarClauses :: [ParsedClause] -> Int -> Expr
desugarClauses [(pats, RhsPlain body)] _
    | all isTrivialPat pats =
        -- Fast path for the Phase-2.0/2.1 classic shape.
        let toName (PVar n) = n
            toName PWild    = "_"           -- preserves lazy ignore
            toName _        = error "impossible"
        in foldr ELam body (map toName pats)
desugarClauses clauses arity =
    let argNames = [BC.pack ("$a" ++ show i) | i <- [0 .. arity - 1]]
        -- Build the body by nesting: outermost is clause 1's attempt,
        -- fallback is clause 2's attempt, etc.
        ultimateFail = EApp (EVar "error")
                            (stringToConsList
                                "Non-exhaustive patterns in function")
        bodyExpr = buildClauses argNames clauses ultimateFail
    in foldr ELam bodyExpr argNames

-- | Compile a list of clauses into one expression with @fallback@ used
-- when every clause fails. Uses a let-binding so each clause's
-- fallback is shared (not duplicated across every pattern mismatch).
buildClauses :: [Name] -> [ParsedClause] -> Expr -> Expr
buildClauses _       []           fallback = fallback
buildClauses argNames (c:cs)      fallback =
    let fresh     = BC.pack ("$fb" ++ show (length cs))
        restExpr  = buildClauses argNames cs fallback
        attempt   = buildOneClause argNames c (EVar fresh)
    in ELet [(fresh, restExpr)] attempt

-- | Compile a single parsed clause. Given the arg-names and the
-- expression to run if this clause fails (pattern-mismatch or all
-- guards false), emit either a plain match body or an if-chain body.
buildOneClause :: [Name] -> ParsedClause -> Expr -> Expr
buildOneClause argNames (pats, rhs) fallback =
    let innerBody = case rhs of
            RhsPlain body    -> body
            RhsGuards branches -> guardChain branches fallback
    in matchPatterns (zip pats argNames) innerBody fallback

-- | Chain guard branches into @if g1 then e1 else if g2 then e2 ... else fb@.
guardChain :: [(Expr, Expr)] -> Expr -> Expr
guardChain []           fb = fb
guardChain ((g, e):rest) fb = EIf g e (guardChain rest fb)

-- | Build a nested @case@ that matches each pattern against its
-- corresponding arg, threading @fallback@ through every no-match.
--
-- For trivial patterns (PVar / PWild) we skip the @case@ and bind
-- via @ELet@ (PVar) or simply discard (PWild). This preserves the
-- laziness that 'case' would otherwise destroy by forcing the arg.
matchPatterns :: [(Pat, Name)] -> Expr -> Expr -> Expr
matchPatterns [] body _ = body
matchPatterns ((p, argName) : rest) body fallback =
    case p of
        PVar n
            | n == "_"  -> matchPatterns rest body fallback
            | otherwise ->
                ELet [(n, EVar argName)] (matchPatterns rest body fallback)
        PWild ->
            matchPatterns rest body fallback
        _ ->
            ECase (EVar argName)
                [ Alt p (matchPatterns rest body fallback)
                , Alt PWild fallback
                ]

isTrivialPat :: Pat -> Bool
isTrivialPat (PVar _) = True
isTrivialPat PWild    = True
isTrivialPat _        = False

--------------------------------------------------------------------------------
-- Where-clause split (token-walk, paren-aware, ignores body-bounds)
--------------------------------------------------------------------------------

splitOnWhere :: Source -> Span -> (Span, Maybe Span)
splitOnWhere src (start, end) = go (Cursor start 1 1) (0 :: Int)
  where
    go cur depth
        | cPos cur >= end = ((start, end), Nothing)
        | otherwise =
            let (tok, cur') = nextToken src cur in
            case tkKind tok of
                TkEof    -> ((start, end), Nothing)
                TkWhere | depth == 0 ->
                    ((start, tkStart tok), Just (tkEnd tok, end))
                TkLParen -> go cur' (depth + 1)
                TkRParen -> go cur' (max 0 (depth - 1))
                _        -> go cur' depth

-- | Parse a sequence of `name = expr` value bindings (braced or layout).
parseBindingsIn :: Source -> Span -> IO [Bind]
parseBindingsIn src (start, end) = do
    let cur0 = Cursor start 1 1
        -- Provisional ctx: refined once we know the binding column.
        provCtx = Ctx src end 0
        (firstTok, curAfter) = nextSig provCtx cur0
    case tkKind firstTok of
        TkLBrace -> braced (Ctx src end 0) curAfter []
        TkEof    -> pure []
        _        ->
            -- Each where/let binding's RHS is bounded by the binding
            -- column: any token at <= bindCol starts a new binding (or
            -- ends the block).
            let ctx = Ctx src end (tkCol firstTok)
            in layout ctx (tkCol firstTok) cur0 []
  where
    parseOne ctx cur = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkIdent n -> pure n
            _         -> parseErr "expected identifier in binding" nameTok
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr "expected `=` in binding" eqTok
        (expr, cur3) <- parseExpr ctx cur2
        pure ((name, expr), cur3)

    braced ctx cur acc
        | cPos cur >= ctxEnd ctx = pure (reverse acc)
        | otherwise = do
            (b, cur') <- parseOne ctx cur
            let (sep, curN) = nextSig ctx cur'
            case tkKind sep of
                TkSemi   -> braced ctx curN (b : acc)
                TkRBrace -> pure (reverse (b : acc))
                TkEof    -> pure (reverse (b : acc))
                _        -> parseErr "expected `;` or `}` in let/where" sep

    layout ctx bindCol cur acc
        | cPos cur >= ctxEnd ctx = pure (reverse acc)
        | otherwise = do
            (b, cur') <- parseOne ctx cur
            let (nextTok, _) = nextSig ctx cur'
            case tkKind nextTok of
                TkEof -> pure (reverse (b : acc))
                _ | tkCol nextTok == bindCol && cPos cur' < ctxEnd ctx ->
                       layout ctx bindCol cur' (b : acc)
                  | otherwise ->
                       pure (reverse (b : acc))

--------------------------------------------------------------------------------
-- nextSig with body-end bound
--------------------------------------------------------------------------------

-- | Get the next significant (non-newline) token, but synthesise a
-- virtual 'TkEof' if the cursor is at or past the body span's end.
-- Without this bound, an expression body would devour the next
-- top-level binding's tokens.
nextSig :: Ctx -> Cursor -> (Token, Cursor)
nextSig ctx cur
    | cPos cur >= ctxEnd ctx =
        (Token TkEof (ctxEnd ctx) (ctxEnd ctx) 0 0, cur)
    | otherwise =
        let (t, c) = nextToken (ctxSrc ctx) cur in
        case tkKind t of
            TkNewline -> nextSig ctx c
            _         -> (t, c)

--------------------------------------------------------------------------------
-- Expression parser
--------------------------------------------------------------------------------

parseExpr :: Ctx -> Cursor -> IO (Expr, Cursor)
parseExpr ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkIf   -> parseIf   ctx cur1
        TkDo   -> parseDo   ctx cur1
        TkLet  -> parseLet  ctx cur1
        TkCase -> parseCase ctx cur1
        _      -> parseOr   ctx cur0

parseIf :: Ctx -> Cursor -> IO (Expr, Cursor)
parseIf ctx cur0 = do
    (c, cur1)  <- parseExpr ctx cur0
    let (t1, cur2) = nextSig ctx cur1
    case tkKind t1 of
        TkThen -> pure ()
        _      -> parseErr "expected `then`" t1
    (t, cur3)  <- parseExpr ctx cur2
    let (t2, cur4) = nextSig ctx cur3
    case tkKind t2 of
        TkElse -> pure ()
        _      -> parseErr "expected `else`" t2
    (e, cur5)  <- parseExpr ctx cur4
    pure (EIf c t e, cur5)

parseDo :: Ctx -> Cursor -> IO (Expr, Cursor)
parseDo ctx cur0 = do
    let (firstTok, curAfter) = nextSig ctx cur0
    case tkKind firstTok of
        TkLBrace -> bracedStmts curAfter []
        TkEof    -> pure (EDo [], cur0)
        _        -> layoutStmts (tkCol firstTok) cur0 []
  where
    bracedStmts cur acc = do
        (e, cur') <- parseExpr ctx cur
        let (sep, curN) = nextSig ctx cur'
        case tkKind sep of
            TkSemi   -> bracedStmts curN (e : acc)
            TkRBrace -> pure (EDo (reverse (e : acc)), curN)
            _        -> parseErr "expected `;` or `}` in do-block" sep

    -- Each stmt's expression sees ctxMinCol = stmtCol so its parseApp
    -- / operator loops won't gobble the next stmt at the same column.
    layoutStmts stmtCol cur acc = do
        let stmtCtx = ctx { ctxMinCol = stmtCol }
        (e, cur') <- parseExpr stmtCtx cur
        let (nextTok, _) = nextSig ctx cur'
        case tkKind nextTok of
            TkEof -> pure (EDo (reverse (e : acc)), cur')
            _ | tkCol nextTok == stmtCol -> layoutStmts stmtCol cur' (e : acc)
              | otherwise                -> pure (EDo (reverse (e : acc)), cur')

parseLet :: Ctx -> Cursor -> IO (Expr, Cursor)
parseLet ctx cur0 = do
    let (firstTok, curAfter) = nextSig ctx cur0
    (binds, curEnd) <- case tkKind firstTok of
        TkLBrace -> bracedBinds curAfter []
        _        -> singleBind cur0
    let (inTok, curIn) = nextSig ctx curEnd
    case tkKind inTok of
        TkIn -> pure ()
        _    -> parseErr "expected `in` in let-binding" inTok
    (body, curBody) <- parseExpr ctx curIn
    pure (ELet binds body, curBody)
  where
    singleBind cur = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkIdent n -> pure n
            _         -> parseErr "expected identifier after `let`" nameTok
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr "expected `=` in let-binding" eqTok
        (e, cur3) <- parseExpr ctx cur2
        pure ([(name, e)], cur3)

    bracedBinds cur acc = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkIdent n -> pure n
            _         -> parseErr "expected identifier in let-binding" nameTok
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr "expected `=` in let-binding" eqTok
        (e, cur3) <- parseExpr ctx cur2
        let (sep, curN) = nextSig ctx cur3
        case tkKind sep of
            TkSemi   -> bracedBinds curN ((name, e) : acc)
            TkRBrace -> pure (reverse ((name, e) : acc), curN)
            _        -> parseErr "expected `;` or `}` in let-block" sep

parseCase :: Ctx -> Cursor -> IO (Expr, Cursor)
parseCase ctx cur0 = do
    (scrut, curS) <- parseAtom ctx cur0
    let (ofTok, curO) = nextSig ctx curS
    case tkKind ofTok of
        TkOf -> pure ()
        _    -> parseErr "expected `of` in case-expression" ofTok
    let (firstTok, curBody) = nextSig ctx curO
    (alts, curEnd) <- case tkKind firstTok of
        TkLBrace -> bracedAlts curBody []
        _        -> layoutAlts (tkCol firstTok) curO []
    pure (ECase scrut alts, curEnd)
  where
    -- The caller-supplied @altCtx@ is used for the RHS expression so
    -- it respects the alt's column boundary (otherwise the RHS would
    -- gobble the next alt).
    parseAlt altCtx cur = do
        (pat, cur1) <- parseTopPat ctx cur
        let (arr, cur2) = nextSig ctx cur1
        case tkKind arr of
            TkArrow -> pure ()
            _       -> parseErr "expected `->` in case alternative" arr
        (e, cur3) <- parseExpr altCtx cur2
        pure (Alt pat e, cur3)

    bracedAlts cur acc = do
        (alt, cur') <- parseAlt ctx cur
        let (sep, curN) = nextSig ctx cur'
        case tkKind sep of
            TkSemi   -> bracedAlts curN (alt : acc)
            TkRBrace -> pure (reverse (alt : acc), curN)
            _        -> parseErr "expected `;` or `}` in case alts" sep

    layoutAlts altCol cur acc = do
        let altCtx = ctx { ctxMinCol = altCol }
        (alt, cur') <- parseAlt altCtx cur
        let (nextTok, _) = nextSig ctx cur'
        case tkKind nextTok of
            TkEof -> pure (reverse (alt : acc), cur')
            _ | tkCol nextTok == altCol ->
                  layoutAlts altCol cur' (alt : acc)
              | otherwise ->
                  pure (reverse (alt : acc), cur')

--------------------------------------------------------------------------------
-- Pattern parsing (shared between case-alts and LHS patterns)
--------------------------------------------------------------------------------

-- | A "top-level" pattern: may include an outer constructor applied
-- to sub-patterns (@Just x@), or an infix cons (@x:xs@).
parseTopPat :: Ctx -> Cursor -> IO (Pat, Cursor)
parseTopPat ctx cur = do
    (p, cur') <- parseTopPatNoCons ctx cur
    consTail p cur'
  where
    consTail p cur0 =
        let (tok, cur1) = nextSig ctx cur0 in
        case tkKind tok of
            TkColon -> do
                (rhs, cur2) <- parseTopPat ctx cur1
                pure (PCon ":" [p, rhs], cur2)
            _ -> pure (p, cur0)

parseTopPatNoCons :: Ctx -> Cursor -> IO (Pat, Cursor)
parseTopPatNoCons ctx cur = do
    let (tok, cur1) = nextSig ctx cur
    case tkKind tok of
        TkConId n  -> collectArgs ctx n [] cur1
        TkLParen   -> do
            (inner, cur2) <- parseTopPat ctx cur1
            let (close, cur3) = nextSig ctx cur2
            case tkKind close of
                TkRParen -> pure (inner, cur3)
                _        -> parseErr "expected `)` in pattern" close
        TkLBracket -> parseListPat ctx cur1
        TkMinus -> do
            -- Negative integer literal pattern, e.g. @-1@.
            let (n, cur2) = nextSig ctx cur1
            case tkKind n of
                TkInt i -> pure (PLit (LInt (fromInteger (negate i))), cur2)
                _       -> parseErr "expected integer after `-` in pattern" n
        _          -> simplePat tok cur1

-- | Gather sub-patterns for an outer constructor pattern until we hit
-- a non-pattern token (like `->`, `=`, `|`, or EOF).
collectArgs :: Ctx -> Name -> [Pat] -> Cursor -> IO (Pat, Cursor)
collectArgs ctx name acc cur =
    let (tok, _) = nextSig ctx cur in
    if startsPat (tkKind tok)
        then do
            (sp, cur') <- parseSubPat ctx cur
            collectArgs ctx name (sp : acc) cur'
        else pure (PCon name (reverse acc), cur)

-- | A sub-pattern — recursive, but a bare TkConId is nullary here.
-- To pass args to a nested constructor, wrap it in parens: @Just (Just x)@.
parseSubPat :: Ctx -> Cursor -> IO (Pat, Cursor)
parseSubPat ctx cur = do
    let (tok, cur1) = nextSig ctx cur
    case tkKind tok of
        TkConId n  -> pure (PCon n [], cur1)
        TkLParen   -> do
            (inner, cur2) <- parseTopPat ctx cur1
            let (close, cur3) = nextSig ctx cur2
            case tkKind close of
                TkRParen -> pure (inner, cur3)
                _        -> parseErr "expected `)` in pattern" close
        TkLBracket -> parseListPat ctx cur1
        TkMinus -> do
            let (n, cur2) = nextSig ctx cur1
            case tkKind n of
                TkInt i -> pure (PLit (LInt (fromInteger (negate i))), cur2)
                _       -> parseErr "expected integer after `-` in pattern" n
        _ -> simplePat tok cur1

-- | Parse a list-literal pattern: `[]`, `[a]`, `[a, b, c]`. The opening
-- `[` has already been consumed.
parseListPat :: Ctx -> Cursor -> IO (Pat, Cursor)
parseListPat ctx cur = do
    let (tok, cur1) = nextSig ctx cur
    case tkKind tok of
        TkRBracket -> pure (PCon "[]" [], cur1)
        _ -> do
            (first, cur2) <- parseSubPat ctx cur
            gatherListPat ctx [first] cur2

gatherListPat :: Ctx -> [Pat] -> Cursor -> IO (Pat, Cursor)
gatherListPat ctx acc cur = do
    let (tok, cur1) = nextSig ctx cur
    case tkKind tok of
        TkComma -> do
            (p, cur2) <- parseSubPat ctx cur1
            gatherListPat ctx (p : acc) cur2
        TkRBracket ->
            let build []     = PCon "[]" []
                build (p:ps) = PCon ":" [p, build ps]
            in pure (build (reverse acc), cur1)
        _ -> parseErr "expected `,` or `]` in list pattern" tok

simplePat :: Token -> Cursor -> IO (Pat, Cursor)
simplePat tok cur1 = case tkKind tok of
    TkInt n      -> pure (PLit (LInt (fromInteger n)), cur1)
    TkStr s      -> pure (PLit (LStr s), cur1)
    TkChar c     -> pure (PLit (LChar c), cur1)
    TkUnderscore -> pure (PWild, cur1)
    TkIdent n    -> pure (PVar n, cur1)
    _ -> parseErr "expected pattern (Int, String, _, ident, or constructor)" tok

startsPat :: TokenKind -> Bool
startsPat (TkInt _)    = True
startsPat (TkChar _)   = True
startsPat (TkStr _)    = True
startsPat (TkIdent _)  = True
startsPat TkUnderscore = True
startsPat (TkConId _)  = True
startsPat TkLParen     = True
startsPat TkLBracket   = True
startsPat TkMinus      = True    -- negative integer literal pattern
startsPat _            = False

--------------------------------------------------------------------------------
-- Operator precedence
--------------------------------------------------------------------------------

parseOr, parseAnd, parseRel, parseCons, parseSum, parseTerm, parseApp, parseAtom
    :: Ctx -> Cursor -> IO (Expr, Cursor)

parseOr  ctx c = chain1L ctx c parseAnd "||" matchesOr
  where matchesOr TkOr = True; matchesOr _ = False
parseAnd ctx c = chain1L ctx c parseRel "&&" matchesAnd
  where matchesAnd TkAnd = True; matchesAnd _ = False

parseRel ctx cur0 = do
    (l, cur1) <- parseCons ctx cur0
    let (tok, curN) = nextSig ctx cur1
    case tkKind tok of
        TkLe   -> binApp "<="  curN l
        TkLt   -> binApp "<"   curN l
        TkGe   -> binApp ">="  curN l
        TkGt   -> binApp ">"   curN l
        TkEqEq -> binApp "=="  curN l
        TkNeq  -> binApp "/="  curN l
        _      -> pure (l, cur1)
  where
    binApp opName cur l = do
        (r, cur') <- parseCons ctx cur
        pure (EApp (EApp (EVar opName) l) r, cur')

-- Cons `:` is right-associative, Haskell precedence 5 (above +,- and ++).
-- Parse sums, then if we see `:`, recursively parse the right-hand side
-- at the same level to get right-associativity.
parseCons ctx cur0 = do
    (l, cur1) <- parseSum ctx cur0
    let (tok, curN) = nextSig ctx cur1
    case tkKind tok of
        TkColon -> do
            (r, cur') <- parseCons ctx curN
            pure (EApp (EApp (EVar ":") l) r, cur')
        _ -> pure (l, cur1)

parseSum ctx cur0 = do
    (l, cur1) <- parseTerm ctx cur0
    loop l cur1
  where
    loop l cur =
        let (tok, curN) = nextSig ctx cur in
        case tkKind tok of
            TkPlus     -> step "+"  curN l
            TkMinus    -> step "-"  curN l
            TkPlusPlus -> step "++" curN l
            _          -> pure (l, cur)
    step op cur l = do
        (r, cur') <- parseTerm ctx cur
        loop (EApp (EApp (EVar op) l) r) cur'

parseTerm ctx cur0 = do
    (l, cur1) <- parseApp ctx cur0
    loop l cur1
  where
    loop l cur =
        let (tok, curN) = nextSig ctx cur in
        case tkKind tok of
            TkStar -> do
                (r, cur') <- parseApp ctx curN
                loop (EApp (EApp (EVar "*") l) r) cur'
            _ -> pure (l, cur)

parseApp ctx cur0 = do
    (head_, cur1) <- parseAtom ctx cur0
    loop head_ cur1
  where
    loop fn cur =
        let (tok, _) = nextSig ctx cur in
        if startsAtom (tkKind tok) && tkCol tok > ctxMinCol ctx
            then do
                (arg, cur') <- parseAtom ctx cur
                loop (EApp fn arg) cur'
            else pure (fn, cur)

    -- A column at or before ctxMinCol marks the start of a new
    -- layout item (next stmt in a do-block, next binding in a
    -- where-clause, next top-level binding for body parsing). It must
    -- NOT be greedily consumed as an additional argument.

    startsAtom TkInt{}        = True
    startsAtom TkStr{}        = True
    startsAtom TkChar{}       = True
    startsAtom TkLParen       = True
    startsAtom TkLBracket     = True
    startsAtom TkIdent{}      = True
    startsAtom TkConId{}      = True
    startsAtom _              = False

parseAtom ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkInt n  -> pure (ELit (LInt (fromInteger n)), cur1)
        TkStr s  -> pure (stringToConsList (BC.unpack s), cur1)
        TkChar c -> pure (ELit (LChar c), cur1)
        TkIdent n
            | n == "_" -> parseErr "wildcard `_` in expression position" tok
            | otherwise -> pure (EVar n, cur1)
        TkConId n -> pure (EVar n, cur1)
        TkLParen -> do
            -- Special case: "(:)" as a section for the cons operator.
            let (peekTok, afterPeek) = nextSig ctx cur1
            case tkKind peekTok of
                TkColon ->
                    let (close, cur3) = nextSig ctx afterPeek in
                    case tkKind close of
                        TkRParen -> pure (EVar ":", cur3)
                        _        -> do
                            (e, cur2) <- parseExpr ctx cur1
                            let (close', cur3') = nextSig ctx cur2
                            case tkKind close' of
                                TkRParen -> pure (e, cur3')
                                _        -> parseErr "expected `)`" close'
                _ -> do
                    (e, cur2) <- parseExpr ctx cur1
                    let (close, cur3) = nextSig ctx cur2
                    case tkKind close of
                        TkRParen -> pure (e, cur3)
                        _        -> parseErr "expected `)`" close
        TkLBracket -> parseListLit ctx cur1
        TkMinus -> do
            (e, cur2) <- parseAtom ctx cur1
            pure (ENeg e, cur2)
        TkEof -> throwIO (ParseError ("empty expression at offset " <> show (tkStart tok)))
        _ -> parseErr "unexpected token" tok

-- | Desugar a string literal into a chain of cons/nil applications.
-- "Hi" becomes @'H' : 'i' : []@ i.e. @EApp (EApp (EVar ":") (ELit (LChar 'H'))) (...)@.
stringToConsList :: String -> Expr
stringToConsList []     = EVar "[]"
stringToConsList (c:cs) = EApp (EApp (EVar ":") (ELit (LChar c))) (stringToConsList cs)

-- | Parse a list literal @[e1, e2, ..., en]@. The opening @[@ has
-- already been consumed; we start at the first element (or @]@ for
-- the empty list). Desugars to a chain of cons: @[a,b] ~~> a : b : []@.
parseListLit :: Ctx -> Cursor -> IO (Expr, Cursor)
parseListLit ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkRBracket -> pure (EVar "[]", cur1)
        _ -> do
            (first, cur2) <- parseExpr ctx cur0
            gather [first] cur2
  where
    gather acc cur = do
        let (tok, cur1) = nextSig ctx cur
        case tkKind tok of
            TkComma -> do
                (e, cur2) <- parseExpr ctx cur1
                gather (e : acc) cur2
            TkRBracket -> pure (buildCons (reverse acc), cur1)
            _ -> parseErr "expected `,` or `]` in list literal" tok

    buildCons :: [Expr] -> Expr
    buildCons []     = EVar "[]"
    buildCons (e:es) = EApp (EApp (EVar ":") e) (buildCons es)

chain1L :: Ctx -> Cursor
        -> (Ctx -> Cursor -> IO (Expr, Cursor))
        -> Name
        -> (TokenKind -> Bool)
        -> IO (Expr, Cursor)
chain1L ctx cur0 sub opName matches = do
    (l, cur1) <- sub ctx cur0
    loop l cur1
  where
    loop l cur =
        let (tok, curN) = nextSig ctx cur in
        if matches (tkKind tok)
            then do
                (r, cur') <- sub ctx curN
                loop (EApp (EApp (EVar opName) l) r) cur'
            else pure (l, cur)

parseErr :: String -> Token -> IO a
parseErr msg tok =
    throwIO (ParseError (msg <> " at offset " <> show (tkStart tok)
                         <> " but saw " <> show (tkKind tok)))
