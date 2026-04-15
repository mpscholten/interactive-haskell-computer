{-# LANGUAGE DeriveAnyClass #-}

-- | Single-pass recursive-descent parser producing AST.
--
-- Phase 2.6 rewrites the operator layer as a Pratt (binding-power)
-- parser driven by a per-module 'FixityTable', so new operators like
-- @<>@, @$@, @>>=@, @.@, @:|@ parse correctly with their standard
-- precedences. User-declared @infixl / infixr / infix N op@ rules are
-- threaded through at the call site.
--
-- New surface syntax supported:
--
--   * Multi-arg lambda @\\x y z -> body@ → nested 'ELam'.
--   * Sections @(+ 1)@, @(1 +)@, @(\`mod\` 5)@, @(10 \`div\`)@.
--   * Lambda-case @\\case { …alts }@.
--   * Multi-way if @if | g -> e | …@.
--   * Backtick infix @x \`f\` y@ with default @infixl 9@ fixity.
--   * Tuples @(a, b, c)@ → 'ETuple'.
--   * As-patterns @xs\@(x:_)@ → 'PAs'.
--   * Bang patterns @!x@ → 'PBang' (ignored strictness).
--   * Tuple patterns → 'PTuple'.
--   * @\$@, @\$!@, @.@, @<>@, @>>=@, @>>@, @<*>@, @<\$>@, @.&.@,
--     @.|.@, @:|@, and arbitrary user symbols participate in the
--     Pratt loop with default fixity.
--   * Type-annotation expressions @(e :: T)@ → e (type swallowed).
--
-- Multi-clause, guards, where, do, let, if, case remain.
module IHC.Parser
    ( parseBodyExpr
    , parseBodyExprWithFixity
    , parseExprOnly
    , ParseError(..)
    , FixityTable
    , Assoc(..)
    , defaultFixityTable
    , scanFixityDecls
    ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import IHC.AST
import IHC.Lexer
import IHC.Scan (Clause(..))
import IHC.Source

newtype ParseError = ParseError String
    deriving stock (Show)
    deriving anyclass (Exception)

--------------------------------------------------------------------------------
-- Fixity table
--------------------------------------------------------------------------------

data Assoc = AssocL | AssocR | AssocN
    deriving (Eq, Show)

-- | Per-operator-name: associativity + precedence (0..9).
type FixityTable = Map Name (Assoc, Int)

-- | Haskell-2010 defaults plus the common extensions that appear across
-- real-world Hackage sources. Unknown operators fall back to 'AssocL'
-- precedence 9 (see 'lookupFixity').
defaultFixityTable :: FixityTable
defaultFixityTable = Map.fromList
    [ ("$",   (AssocR, 0))
    , ("$!",  (AssocR, 0))
    , ("`seq`", (AssocR, 0))
    , (">>",  (AssocL, 1))
    , (">>=", (AssocL, 1))
    , ("=<<", (AssocR, 1))
    , ("||",  (AssocR, 2))
    , ("&&",  (AssocR, 3))
    , ("==",  (AssocN, 4))
    , ("/=",  (AssocN, 4))
    , ("<",   (AssocN, 4))
    , ("<=",  (AssocN, 4))
    , (">",   (AssocN, 4))
    , (">=",  (AssocN, 4))
    , ("<$>", (AssocL, 4))
    , ("<$",  (AssocL, 4))
    , ("<*>", (AssocL, 4))
    , ("<*",  (AssocL, 4))
    , ("*>",  (AssocL, 4))
    , ("<|>", (AssocL, 3))
    , (":",   (AssocR, 5))
    , ("++",  (AssocR, 5))
    , (":|",  (AssocR, 5))
    , ("<>",  (AssocR, 6))
    , ("+",   (AssocL, 6))
    , ("-",   (AssocL, 6))
    , ("*",   (AssocL, 7))
    , ("/",   (AssocL, 7))
    , ("`div`", (AssocL, 7))
    , ("`mod`", (AssocL, 7))
    , ("`quot`", (AssocL, 7))
    , ("`rem`", (AssocL, 7))
    , (".&.", (AssocL, 7))
    , (".|.", (AssocL, 5))
    , ("^",   (AssocR, 8))
    , ("^^",  (AssocR, 8))
    , ("**",  (AssocR, 8))
    , (".",   (AssocR, 9))
    , ("!!",  (AssocL, 9))
    ]

lookupFixity :: FixityTable -> Name -> (Assoc, Int)
lookupFixity tbl name = case Map.lookup name tbl of
    Just fx -> fx
    Nothing -> (AssocL, 9)        -- default for user-defined operators

-- | Scan a whole source file for top-level @infixl / infixr / infix N op@
-- declarations, extending the starting table. Used by the scheduler
-- BEFORE function bodies are parsed, so the body parser sees the final
-- fixity table.
--
-- The scan is lexer-level: it never builds an AST. Declarations not at
-- column 1 are ignored.
scanFixityDecls :: Source -> FixityTable -> IO FixityTable
scanFixityDecls src tbl0 = go tbl0 startCursor
  where
    go acc cur = do
        let (tok, cur') = nextToken src cur
        case tkKind tok of
            TkEof        -> pure acc
            TkInfixL | tkCol tok == 1 -> grab AssocL acc cur'
            TkInfixR | tkCol tok == 1 -> grab AssocR acc cur'
            TkInfix  | tkCol tok == 1 -> grab AssocN acc cur'
            _ -> go acc cur'

    grab assoc acc cur = do
        let (precTok, cur1) = skipNewlines cur
        case tkKind precTok of
            TkInt n -> consumeOps assoc (fromInteger n) acc (advance cur1)
            _ -> go acc cur

    advance cur = snd (nextToken src cur)

    -- After `infixl N`, collect a comma-separated list of op names
    -- until we leave the line (any column-1 token or EOF).
    consumeOps assoc prec acc cur = do
        let (tok, cur1) = nextToken src cur
        case tkKind tok of
            TkEof      -> pure acc
            TkNewline  ->
                let (pk, _) = skipNewlines cur1 in
                case tkKind pk of
                    TkEof -> pure acc
                    _ | tkCol pk == 1 -> go acc cur1
                      | otherwise     -> consumeOps assoc prec acc cur1
            TkComma    -> consumeOps assoc prec acc cur1
            TkBacktick ->
                let (idTok, cur2) = nextToken src cur1
                    (_closeTok, cur3) = nextToken src cur2
                in case tkKind idTok of
                    TkIdent n ->
                        let key = BC.pack "`" <> n <> BC.pack "`"
                        in consumeOps assoc prec (Map.insert key (assoc, prec) acc) cur3
                    _ -> consumeOps assoc prec acc cur2
            TkSymOp n  -> consumeOps assoc prec (Map.insert n (assoc, prec) acc) cur1
            TkPlus     -> consumeOps assoc prec (Map.insert "+" (assoc, prec) acc) cur1
            TkMinus    -> consumeOps assoc prec (Map.insert "-" (assoc, prec) acc) cur1
            TkStar     -> consumeOps assoc prec (Map.insert "*" (assoc, prec) acc) cur1
            TkPlusPlus -> consumeOps assoc prec (Map.insert "++" (assoc, prec) acc) cur1
            TkColon    -> consumeOps assoc prec (Map.insert ":" (assoc, prec) acc) cur1
            TkDollar   -> consumeOps assoc prec (Map.insert "$" (assoc, prec) acc) cur1
            TkDot      -> consumeOps assoc prec (Map.insert "." (assoc, prec) acc) cur1
            _          -> go acc cur1

    skipNewlines cur =
        let (t, c) = nextToken src cur in
        case tkKind t of
            TkNewline -> skipNewlines c
            _         -> (t, c)

--------------------------------------------------------------------------------
-- Parser context
--------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxSrc    :: !Source
    , ctxEnd    :: !Pos
    , ctxMinCol :: !Int
    , ctxFixity :: !FixityTable
    }

-- | Parse all clauses with the default fixity table. Kept so existing
-- scheduler call sites keep working without passing a table.
parseBodyExpr :: Source -> [Clause] -> IO Expr
parseBodyExpr src = parseBodyExprWithFixity src defaultFixityTable

parseBodyExprWithFixity :: Source -> FixityTable -> [Clause] -> IO Expr
parseBodyExprWithFixity _   _ [] = throwIO (ParseError "parseBodyExpr: empty clause list")
parseBodyExprWithFixity src fx clauses = do
    parsed <- mapM (parseClause src fx) clauses
    let arity = case parsed of
            ((ps, _) : _) -> length ps
            _             -> 0
    mapM_ (\(ps, _) ->
                if length ps == arity
                    then pure ()
                    else throwIO (ParseError
                        "parseBodyExpr: clauses have differing arities"))
          parsed
    pure (desugarClauses parsed arity)

-- | Parse a single expression from raw bytes — intended for REPL input.
-- The entire source is treated as one expression (no binding LHS, no `=`).
-- Throws 'ParseError' on failure.
parseExprOnly :: Source -> FixityTable -> IO Expr
parseExprOnly src fx = do
    let end = BC.length (srcBytes src)
        ctx = Ctx src end 0 fx
        cur = startCursor
    (e, _) <- parseExpr ctx cur
    pure e

--------------------------------------------------------------------------------
-- Parsed clauses
--------------------------------------------------------------------------------

type ParsedClause = ([Pat], Rhs)

data Rhs
    = RhsPlain  !Expr
    | RhsGuards ![(Expr, Expr)]

parseClause :: Source -> FixityTable -> Clause -> IO ParsedClause
parseClause src fx (Clause patsSpan rhsSpan) = do
    let (coreSpan, mWhere) = splitOnWhere src rhsSpan
    whereBinds <- case mWhere of
        Nothing -> pure []
        Just ws -> parseBindingsIn src fx ws

    pats <- parsePatsIn src fx patsSpan
    rhs  <- parseRhsIn src fx coreSpan
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

parsePatsIn :: Source -> FixityTable -> Span -> IO [Pat]
parsePatsIn src fx (start, end) = do
    let ctx  = Ctx src end 0 fx
        cur0 = Cursor start 1 1
    loop ctx cur0 []
  where
    loop ctx cur acc = do
        let (tok, _) = nextSig ctx cur
        if tkKind tok == TkEof || not (startsPat (tkKind tok))
            then pure (reverse acc)
            else do
                (p, cur') <- parseSubPat ctx cur
                loop ctx cur' (p : acc)

parseRhsIn :: Source -> FixityTable -> Span -> IO Rhs
parseRhsIn src fx (start, end) = do
    let ctx  = Ctx src end 1 fx
        cur0 = Cursor start 1 1
        (firstTok, cur1) = nextSig ctx cur0
    case tkKind firstTok of
        TkEq -> do
            (e, _) <- parseExpr ctx cur1
            pure (RhsPlain e)
        TkBar -> do
            branches <- parseGuards ctx cur1 []
            pure (RhsGuards branches)
        _ -> parseErr "expected `=` or `|` at start of RHS" firstTok
  where
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

desugarClauses :: [ParsedClause] -> Int -> Expr
desugarClauses [(pats, RhsPlain body)] _
    | all isTrivialPat pats =
        let toName (PVar n)  = n
            toName PWild     = "_"
            toName (PBang p) = toName p
            toName _         = error "impossible"
        in foldr ELam body (map toName pats)
desugarClauses clauses arity =
    let argNames = [BC.pack ("$a" ++ show i) | i <- [0 .. arity - 1]]
        ultimateFail = EApp (EVar "error")
                            (stringToConsList
                                "Non-exhaustive patterns in function")
        bodyExpr = buildClauses argNames clauses ultimateFail
    in foldr ELam bodyExpr argNames

buildClauses :: [Name] -> [ParsedClause] -> Expr -> Expr
buildClauses _       []           fallback = fallback
buildClauses argNames (c:cs)      fallback =
    let fresh     = BC.pack ("$fb" ++ show (length cs))
        restExpr  = buildClauses argNames cs fallback
        attempt   = buildOneClause argNames c (EVar fresh)
    in ELet [(fresh, restExpr)] attempt

buildOneClause :: [Name] -> ParsedClause -> Expr -> Expr
buildOneClause argNames (pats, rhs) fallback =
    let innerBody = case rhs of
            RhsPlain body    -> body
            RhsGuards branches -> guardChain branches fallback
    in matchPatterns (zip pats argNames) innerBody fallback

guardChain :: [(Expr, Expr)] -> Expr -> Expr
guardChain []           fb = fb
guardChain ((g, e):rest) fb = EIf g e (guardChain rest fb)

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
        PBang inner -> matchPatterns ((inner, argName) : rest) body fallback
        _ ->
            ECase (EVar argName)
                [ Alt p (matchPatterns rest body fallback)
                , Alt PWild fallback
                ]

isTrivialPat :: Pat -> Bool
isTrivialPat (PVar _) = True
isTrivialPat PWild    = True
isTrivialPat (PBang p) = isTrivialPat p
isTrivialPat _        = False

--------------------------------------------------------------------------------
-- Where-clause split
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

parseBindingsIn :: Source -> FixityTable -> Span -> IO [Bind]
parseBindingsIn src fx (start, end) = do
    let cur0 = Cursor start 1 1
        provCtx = Ctx src end 0 fx
        (firstTok, curAfter) = nextSig provCtx cur0
    case tkKind firstTok of
        TkLBrace -> braced (Ctx src end 0 fx) curAfter []
        TkEof    -> pure []
        _        ->
            let ctx = Ctx src end (tkCol firstTok) fx
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
-- Top-level expression parser — routes on keyword, else Pratt.
--------------------------------------------------------------------------------

parseExpr :: Ctx -> Cursor -> IO (Expr, Cursor)
parseExpr ctx cur0 = do
    (e, cur1) <- parseExprNoSig ctx cur0
    -- Swallow an optional trailing @:: type@ annotation. The value
    -- survives; the type is discarded (we don't type-check).
    let (tok, cur2) = nextSig ctx cur1
    case tkKind tok of
        TkDColon -> do
            cur3 <- skipTypeToBinding ctx cur2
            pure (e, cur3)
        _ -> pure (e, cur1)

parseExprNoSig :: Ctx -> Cursor -> IO (Expr, Cursor)
parseExprNoSig ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkIf        -> parseIf ctx cur1
        TkDo        -> parseDo ctx cur1
        TkLet       -> parseLet ctx cur1
        TkCase      -> parseCase ctx cur1
        TkBackslash -> parseLambda ctx cur1
        _           -> parseBinOp ctx 0 cur0

-- | After @::@ in an expression context, skip tokens until we reach a
-- boundary that ends the enclosing expression. We stop at @,@, @;@,
-- @)@, @]@, @}@ at depth 0, at keyword @in@ (for let), or at EOF.
skipTypeToBinding :: Ctx -> Cursor -> IO Cursor
skipTypeToBinding ctx cur0 = go cur0 (0 :: Int) (0 :: Int) (0 :: Int)
  where
    go cur b p c = do
        let (tok, cur') = nextSig ctx cur
        case tkKind tok of
            TkEof -> pure cur
            TkRParen | b == 0 && p == 0 && c == 0 -> pure cur   -- don't consume
            TkRBracket | b == 0 && p == 0 && c == 0 -> pure cur
            TkRBrace | b == 0 && p == 0 && c == 0 -> pure cur
            TkComma | b == 0 && p == 0 && c == 0 -> pure cur
            TkSemi  | b == 0 && p == 0 && c == 0 -> pure cur
            TkIn    | b == 0 && p == 0 && c == 0 -> pure cur
            TkEq    | b == 0 && p == 0 && c == 0 -> pure cur
            TkLParen   -> go cur' b (p + 1) c
            TkRParen   -> go cur' b (p - 1) c
            TkLBracket -> go cur' (b + 1) p c
            TkRBracket -> go cur' (b - 1) p c
            TkLBrace   -> go cur' b p (c + 1)
            TkRBrace   -> go cur' b p (c - 1)
            _          -> go cur' b p c

-- | Multi-way if: @if | g -> e | g -> e ...@. Assumes @if@ already
-- consumed. Desugars to a nested 'EIf'. Standard @if c then t else e@
-- is handled as before. Distinguishes by peeking for @|@.
parseIf :: Ctx -> Cursor -> IO (Expr, Cursor)
parseIf ctx cur0 = do
    let (peek, curP) = nextSig ctx cur0
    case tkKind peek of
        TkBar -> parseMultiWayIf ctx curP []
        _     -> parseRegularIf ctx cur0
  where
    parseRegularIf c2 cc = do
        (cond, cur1) <- parseExpr c2 cc
        let (t1, cur2) = nextSig c2 cur1
        case tkKind t1 of
            TkThen -> pure ()
            _      -> parseErr "expected `then`" t1
        (th, cur3) <- parseExpr c2 cur2
        let (t2, cur4) = nextSig c2 cur3
        case tkKind t2 of
            TkElse -> pure ()
            _      -> parseErr "expected `else`" t2
        (el, cur5) <- parseExpr c2 cur4
        pure (EIf cond th el, cur5)

    parseMultiWayIf c2 cur acc = do
        (g, cur1) <- parseExpr c2 cur
        let (arr, cur2) = nextSig c2 cur1
        case tkKind arr of
            TkArrow -> pure ()
            _       -> parseErr "expected `->` in multi-way if" arr
        (b, cur3) <- parseExpr c2 cur2
        let (sep, cur4) = nextSig c2 cur3
        case tkKind sep of
            TkBar -> parseMultiWayIf c2 cur4 ((g, b) : acc)
            _     ->
                let branches = reverse ((g, b) : acc)
                    fallback = EApp (EVar "error")
                                    (stringToConsList
                                        "multi-way if: no branch matched")
                in pure (guardChain branches fallback, cur3)

--------------------------------------------------------------------------------
-- Lambda (multi-arg) and lambda-case
--------------------------------------------------------------------------------

parseLambda :: Ctx -> Cursor -> IO (Expr, Cursor)
parseLambda ctx cur0 = do
    let (peek, curP) = nextSig ctx cur0
    case tkKind peek of
        TkCase -> parseLambdaCase ctx curP
        _      -> do
            -- Collect ≥1 patterns until we see `->`.
            (pats, curBody) <- collectLamPats ctx cur0 []
            (body, cur1)    <- parseExpr ctx curBody
            pure (foldr wrapLam body pats, cur1)
  where
    wrapLam (PVar n) e = ELam n e
    wrapLam PWild    e = ELam "_" e
    wrapLam p        e =
        -- Non-trivial pattern inside a lambda: desugar to a case on a
        -- fresh name.
        let n = "$lam"
        in ELam n (ECase (EVar n)
                    [ Alt p e
                    , Alt PWild (EApp (EVar "error")
                                 (stringToConsList
                                     "Non-exhaustive patterns in lambda"))
                    ])

collectLamPats :: Ctx -> Cursor -> [Pat] -> IO ([Pat], Cursor)
collectLamPats ctx cur acc = do
    let (tok, _) = nextSig ctx cur
    case tkKind tok of
        TkArrow ->
            if null acc
                then parseErr "expected at least one pattern in lambda" tok
                else do
                    let (_, curAfter) = nextSig ctx cur
                    pure (reverse acc, curAfter)
        _ | startsPat (tkKind tok) -> do
            (p, cur') <- parseSubPat ctx cur
            collectLamPats ctx cur' (p : acc)
          | otherwise ->
            parseErr "expected pattern or `->` in lambda" tok

-- | \case { alts }  or  \case <layout alts>.  Desugars to
-- @\\$lc -> case $lc of { alts }@.
parseLambdaCase :: Ctx -> Cursor -> IO (Expr, Cursor)
parseLambdaCase ctx cur0 = do
    let (firstTok, curBody) = nextSig ctx cur0
    (alts, curEnd) <- case tkKind firstTok of
        TkLBrace -> bracedAlts ctx curBody []
        _        -> layoutAlts ctx (tkCol firstTok) cur0 []
    let n = "$lc"
    pure (ELam n (ECase (EVar n) alts), curEnd)

--------------------------------------------------------------------------------
-- do / let / case (kept close to the Phase 2.5 code)
--------------------------------------------------------------------------------

parseDo :: Ctx -> Cursor -> IO (Expr, Cursor)
parseDo ctx cur0 = do
    let (firstTok, curAfter) = nextSig ctx cur0
    case tkKind firstTok of
        TkLBrace -> bracedStmts curAfter []
        TkEof    -> pure (EDo [], cur0)
        _        -> layoutStmts (tkCol firstTok) cur0 []
  where
    bracedStmts cur acc = do
        (s, cur') <- parseStmt ctx cur
        let (sep, curN) = nextSig ctx cur'
        case tkKind sep of
            TkSemi   -> bracedStmts curN (s : acc)
            TkRBrace -> pure (EDo (reverse (s : acc)), curN)
            _        -> parseErr "expected `;` or `}` in do-block" sep

    layoutStmts stmtCol cur acc = do
        let stmtCtx = ctx { ctxMinCol = stmtCol }
        (s, cur') <- parseStmt stmtCtx cur
        let (nextTok, _) = nextSig ctx cur'
        case tkKind nextTok of
            TkEof -> pure (EDo (reverse (s : acc)), cur')
            _ | tkCol nextTok == stmtCol -> layoutStmts stmtCol cur' (s : acc)
              | otherwise                -> pure (EDo (reverse (s : acc)), cur')

parseStmt :: Ctx -> Cursor -> IO (Stmt, Cursor)
parseStmt ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkLet -> parseDoLet ctx cur1
        TkIdent name -> do
            let (peek, cur2) = nextSig ctx cur1
            case tkKind peek of
                TkLArrow -> do
                    (e, cur3) <- parseExpr ctx cur2
                    pure (SBind name e, cur3)
                _ -> do
                    (e, cur') <- parseExpr ctx cur0
                    pure (SExpr e, cur')
        _ -> do
            (e, cur') <- parseExpr ctx cur0
            pure (SExpr e, cur')

parseDoLet :: Ctx -> Cursor -> IO (Stmt, Cursor)
parseDoLet ctx cur0 = do
    let (firstTok, curAfter) = nextSig ctx cur0
    (binds, curEnd) <- case tkKind firstTok of
        TkLBrace -> bracedBinds curAfter []
        _        -> singleBind cur0
    pure (SLet binds, curEnd)
  where
    singleBind cur = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkIdent n -> pure n
            _         -> parseErr "expected identifier after `let`" nameTok
        -- Allow curried params before `=`, e.g. `let f x y = ...`.
        (params, cur2) <- collectLetParams ctx cur1 []
        let (eqTok, cur3) = nextSig ctx cur2
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr "expected `=` in let-binding" eqTok
        (e, cur4) <- parseExpr ctx cur3
        pure ([(name, wrapParams params e)], cur4)

    bracedBinds cur acc = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkIdent n -> pure n
            _         -> parseErr "expected identifier in let-binding" nameTok
        (params, cur2) <- collectLetParams ctx cur1 []
        let (eqTok, cur3) = nextSig ctx cur2
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr "expected `=` in let-binding" eqTok
        (e, cur4) <- parseExpr ctx cur3
        let (sep, curN) = nextSig ctx cur4
        case tkKind sep of
            TkSemi   -> bracedBinds curN ((name, wrapParams params e) : acc)
            TkRBrace -> pure (reverse ((name, wrapParams params e) : acc), curN)
            _        -> parseErr "expected `;` or `}` in let-block" sep

-- | Walk forward collecting parameter patterns between a binder name
-- and the @=@ in a let-binding. Empty is legal — a plain @let x = e@.
collectLetParams :: Ctx -> Cursor -> [Pat] -> IO ([Pat], Cursor)
collectLetParams ctx cur acc = do
    let (tok, _) = nextSig ctx cur
    case tkKind tok of
        TkEq -> pure (reverse acc, cur)
        _ | startsPat (tkKind tok) -> do
            (p, cur') <- parseSubPat ctx cur
            collectLetParams ctx cur' (p : acc)
          | otherwise -> pure (reverse acc, cur)

-- | Wrap an expression body in nested lambdas, one per parameter.
wrapParams :: [Pat] -> Expr -> Expr
wrapParams ps body = foldr wrap body ps
  where
    wrap (PVar n)  e = ELam n e
    wrap PWild     e = ELam "_" e
    wrap (PBang p) e = wrap p e
    wrap p         e =
        let n = "$p" in
        ELam n (ECase (EVar n)
                 [ Alt p e
                 , Alt PWild (EApp (EVar "error")
                               (stringToConsList
                                   "Non-exhaustive patterns in let"))
                 ])

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
        -- Allow curried params before `=`, e.g. `let f x y = ...`.
        (params, cur2) <- collectLetParams ctx cur1 []
        let (eqTok, cur3) = nextSig ctx cur2
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr "expected `=` in let-binding" eqTok
        (e, cur4) <- parseExpr ctx cur3
        pure ([(name, wrapParams params e)], cur4)

    bracedBinds cur acc = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkIdent n -> pure n
            _         -> parseErr "expected identifier in let-binding" nameTok
        (params, cur2) <- collectLetParams ctx cur1 []
        let (eqTok, cur3) = nextSig ctx cur2
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr "expected `=` in let-binding" eqTok
        (e, cur4) <- parseExpr ctx cur3
        let (sep, curN) = nextSig ctx cur4
        case tkKind sep of
            TkSemi   -> bracedBinds curN ((name, wrapParams params e) : acc)
            TkRBrace -> pure (reverse ((name, wrapParams params e) : acc), curN)
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
        TkLBrace -> bracedAlts ctx curBody []
        _        -> layoutAlts ctx (tkCol firstTok) curO []
    pure (ECase scrut alts, curEnd)

bracedAlts :: Ctx -> Cursor -> [Alt] -> IO ([Alt], Cursor)
bracedAlts ctx cur acc = do
    (alt, cur') <- parseAlt ctx ctx cur
    let (sep, curN) = nextSig ctx cur'
    case tkKind sep of
        TkSemi   -> bracedAlts ctx curN (alt : acc)
        TkRBrace -> pure (reverse (alt : acc), curN)
        _        -> parseErr "expected `;` or `}` in case alts" sep

layoutAlts :: Ctx -> Int -> Cursor -> [Alt] -> IO ([Alt], Cursor)
layoutAlts ctx altCol cur acc = do
    let altCtx = ctx { ctxMinCol = altCol }
    (alt, cur') <- parseAlt ctx altCtx cur
    let (nextTok, _) = nextSig ctx cur'
    case tkKind nextTok of
        TkEof -> pure (reverse (alt : acc), cur')
        _ | tkCol nextTok == altCol ->
              layoutAlts ctx altCol cur' (alt : acc)
          | otherwise ->
              pure (reverse (alt : acc), cur')

parseAlt :: Ctx -> Ctx -> Cursor -> IO (Alt, Cursor)
parseAlt ctx altCtx cur = do
    (pat, cur1) <- parseTopPat ctx cur
    let (arr, cur2) = nextSig ctx cur1
    case tkKind arr of
        TkArrow -> pure ()
        _       -> parseErr "expected `->` in case alternative" arr
    (e, cur3) <- parseExpr altCtx cur2
    pure (Alt pat e, cur3)

--------------------------------------------------------------------------------
-- Pattern parsing
--------------------------------------------------------------------------------

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
        _          -> parseSubPat ctx cur

collectArgs :: Ctx -> Name -> [Pat] -> Cursor -> IO (Pat, Cursor)
collectArgs ctx name acc cur =
    let (tok, _) = nextSig ctx cur in
    if startsPat (tkKind tok)
        then do
            (sp, cur') <- parseSubPat ctx cur
            collectArgs ctx name (sp : acc) cur'
        else pure (PCon name (reverse acc), cur)

-- | A sub-pattern. Handles as-patterns @name\@sub@, bang patterns,
-- tuples, negative-literal patterns, etc.
parseSubPat :: Ctx -> Cursor -> IO (Pat, Cursor)
parseSubPat ctx cur = do
    let (tok, cur1) = nextSig ctx cur
    case tkKind tok of
        TkBang -> do
            (p, curN) <- parseSubPat ctx cur1
            pure (PBang p, curN)
        TkIdent n -> do
            -- Potential as-pattern: ident '@' sub
            let (peek, curP) = nextSig ctx cur1
            case tkKind peek of
                TkAt -> do
                    (sub, curN) <- parseSubPat ctx curP
                    pure (PAs n sub, curN)
                _    -> pure (PVar n, cur1)
        TkPrimId n   -> pure (PVar n, cur1)   -- e.g. state# param in primop binding
        TkUnderscore -> pure (PWild, cur1)
        TkInt n      -> pure (PLit (LInt (fromInteger n)), cur1)
        TkStr s      -> pure (PLit (LStr s), cur1)
        TkChar c     -> pure (PLit (LChar c), cur1)
        TkConId n    -> pure (PCon n [], cur1)
        TkLParen     -> parseParenPat ctx cur1
        TkLBracket   -> parseListPat ctx cur1
        TkMinus -> do
            let (n, cur2) = nextSig ctx cur1
            case tkKind n of
                TkInt i -> pure (PLit (LInt (fromInteger (negate i))), cur2)
                _       -> parseErr "expected integer after `-` in pattern" n
        _ -> parseErr "expected pattern (Int, String, _, ident, or constructor)" tok

-- | Parenthesised pattern: could be a single pattern @(p)@, a tuple
-- @(p,q,r)@, or unit @()@. The opening @(@ has been consumed.
parseParenPat :: Ctx -> Cursor -> IO (Pat, Cursor)
parseParenPat ctx cur0 = do
    let (peek, curP) = nextSig ctx cur0
    case tkKind peek of
        TkRParen -> pure (PCon "()" [], curP)
        _ -> do
            (first, cur1) <- parseTopPat ctx cur0
            let (sep, cur2) = nextSig ctx cur1
            case tkKind sep of
                TkRParen -> pure (first, cur2)
                TkComma  -> gatherTuple ctx [first] cur2
                _        -> parseErr "expected `)` or `,` in pattern" sep

gatherTuple :: Ctx -> [Pat] -> Cursor -> IO (Pat, Cursor)
gatherTuple ctx acc cur = do
    (p, cur1) <- parseTopPat ctx cur
    let (sep, cur2) = nextSig ctx cur1
    case tkKind sep of
        TkComma  -> gatherTuple ctx (p : acc) cur2
        TkRParen -> pure (PTuple (reverse (p : acc)), cur2)
        _        -> parseErr "expected `,` or `)` in tuple pattern" sep

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

startsPat :: TokenKind -> Bool
startsPat (TkInt _)    = True
startsPat (TkChar _)   = True
startsPat (TkStr _)    = True
startsPat (TkIdent _)  = True
startsPat (TkPrimId _) = True
startsPat TkUnderscore = True
startsPat (TkConId _)  = True
startsPat TkLParen     = True
startsPat TkLBracket   = True
startsPat TkMinus      = True
startsPat TkBang       = True
startsPat _            = False

--------------------------------------------------------------------------------
-- Pratt operator parser
--
-- Consume an expression whose minimum binding power is @minBp@. Loop:
-- parse one prefix/left operand, then peek at the next token. If it's
-- an operator whose left-binding-power ≥ @minBp@, consume + recurse
-- with the right-binding-power as the next floor; otherwise stop.
--------------------------------------------------------------------------------

parseBinOp :: Ctx -> Int -> Cursor -> IO (Expr, Cursor)
parseBinOp ctx minBp cur0 = do
    (l0, cur1) <- parseUnary ctx cur0
    loop l0 cur1
  where
    loop l cur =
        case peekOp ctx cur of
            Nothing             -> pure (l, cur)
            Just (opName, assoc, prec, cur') ->
                if prec < minBp
                    then pure (l, cur)
                    else
                        -- Guard against a "dangling" operator whose RHS
                        -- can't start an expression — this happens inside
                        -- sections @(e op)@ and for GHC magic @#@ suffixes
                        -- like @"hello"#@. In those cases the outer
                        -- context (paren matcher) handles the operator
                        -- consumption; we stop here.
                        if not (rhsCanStart ctx cur')
                            then pure (l, cur)
                            else do
                                let nextMin = case assoc of
                                        AssocL -> prec + 1
                                        AssocR -> prec
                                        AssocN -> prec + 1
                                (r, curR) <- parseBinOp ctx nextMin cur'
                                let l' = applyOp opName l r
                                loop l' curR

    -- Peek whether the token immediately after the operator can start
    -- a fresh expression. Handles `-` (unary minus), lambdas, keywords.
    rhsCanStart c cur =
        let (t, _) = nextSig c cur in
        case tkKind t of
            TkMinus      -> True
            TkBackslash  -> True
            TkIf         -> True
            TkDo         -> True
            TkLet        -> True
            TkCase       -> True
            k            -> startsAtom k

    applyOp "$" a b    = EApp a b
    applyOp "$!" a b   = EApp (EApp (EVar "seq") a) (EApp (EVar "$!") b)
        -- Only kept as a fallback; the builtins expose `$!` directly.
    applyOp name a b   = EApp (EApp (EVar name) a) b

-- | Peek at the next significant token, decide whether it's usable as a
-- binary operator. Returns @(name, assoc, prec, cursor-after-op)@.
-- For backticks, name is the bare function name; for symbolic ops, the
-- bytes of the operator.
--
-- The `ctxMinCol` check is conservative: we still require operator
-- continuation at the same indentation level or deeper, otherwise the
-- operator belongs to the outer construct.
peekOp :: Ctx -> Cursor -> Maybe (Name, Assoc, Int, Cursor)
peekOp ctx cur =
    let (tok, cur') = nextSig ctx cur in
    case tkKind tok of
        TkBacktick ->
            -- Read ident, closing backtick.
            let (idTok, curId) = nextSig ctx cur'
            in case tkKind idTok of
                TkIdent n ->
                    let (closeTok, curClose) = nextSig ctx curId in
                    case tkKind closeTok of
                        TkBacktick ->
                            let key      = BC.pack "`" <> n <> BC.pack "`"
                                (a, p)   = lookupFixity (ctxFixity ctx) key
                            in Just (n, a, p, curClose)
                        _ -> Nothing
                TkConId n ->
                    let (closeTok, curClose) = nextSig ctx curId in
                    case tkKind closeTok of
                        TkBacktick ->
                            let key      = BC.pack "`" <> n <> BC.pack "`"
                                (a, p)   = lookupFixity (ctxFixity ctx) key
                            in Just (n, a, p, curClose)
                        _ -> Nothing
                _ -> Nothing
        _ -> case tokenOpName (tkKind tok) of
            Nothing   -> Nothing
            Just name ->
                let (a, p) = lookupFixity (ctxFixity ctx) name
                in Just (name, a, p, cur')

-- | If this token is usable as a binary operator, return its canonical
-- name (the key used in the fixity table). Minus is tricky — we let it
-- through; unary-minus is handled in 'parseUnary'.
tokenOpName :: TokenKind -> Maybe Name
tokenOpName = \case
    TkPlus     -> Just "+"
    TkMinus    -> Just "-"
    TkStar     -> Just "*"
    TkPlusPlus -> Just "++"
    TkEqEq     -> Just "=="
    TkNeq      -> Just "/="
    TkLt       -> Just "<"
    TkLe       -> Just "<="
    TkGt       -> Just ">"
    TkGe       -> Just ">="
    TkAnd      -> Just "&&"
    TkOr       -> Just "||"
    TkColon    -> Just ":"
    TkDot      -> Just "."
    TkDollar   -> Just "$"
    TkSymOp n  -> Just n
    _          -> Nothing

--------------------------------------------------------------------------------
-- Unary (leading `-`) + application layer
--------------------------------------------------------------------------------

parseUnary :: Ctx -> Cursor -> IO (Expr, Cursor)
parseUnary ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkMinus -> do
            (e, cur2) <- parseUnary ctx cur1
            pure (ENeg e, cur2)
        TkIf         -> parseIf         ctx cur1
        TkDo         -> parseDo         ctx cur1
        TkLet        -> parseLet        ctx cur1
        TkCase       -> parseCase       ctx cur1
        TkBackslash  -> parseLambda     ctx cur1
        _ -> parseApp ctx cur0

parseApp :: Ctx -> Cursor -> IO (Expr, Cursor)
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

startsAtom :: TokenKind -> Bool
startsAtom TkInt{}    = True
startsAtom TkStr{}    = True
startsAtom TkChar{}   = True
startsAtom TkLParen   = True
startsAtom TkLBracket = True
startsAtom TkIdent{}  = True
startsAtom TkConId{}  = True
startsAtom TkPrimId{} = True
startsAtom TkLUnbox   = True
startsAtom _          = False

--------------------------------------------------------------------------------
-- Atoms
--------------------------------------------------------------------------------

parseAtom :: Ctx -> Cursor -> IO (Expr, Cursor)
parseAtom ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkInt n  -> pure (ELit (LInt (fromInteger n)), cur1)
        TkStr s  -> pure (stringToConsList (BC.unpack s), cur1)
        TkChar c -> pure (ELit (LChar c), cur1)
        TkIdent n
            | n == "_" -> parseErr "wildcard `_` in expression position" tok
            | otherwise -> pure (EVar n, cur1)
        TkPrimId n -> pure (EVar n, cur1)
        TkConId n ->
            tryQualified ctx n tok cur1
        TkLParen   -> parseParenExpr ctx tok cur1
        TkLUnbox   -> parseUnboxedTuple ctx cur1
        TkLBracket -> parseListLit ctx cur1
        TkEof -> throwIO (ParseError ("empty expression at offset " <> show (tkStart tok)))
        _ -> parseErr "unexpected token" tok

-- | Dispatcher for everything that lives inside @( ... )@. Covers:
--
--   * @()@ — unit.
--   * @(op)@ — operator as value @(+), (:)@, @(-)@.
--   * @(e)@ — parenthesised expression.
--   * @(e, f, ...)@ — tuple expression.
--   * @(e :: T)@ — type annotation; the expression survives, the type
--     is swallowed up to the matching @)@.
--   * @(op e)@ — right-section, desugars to @\\$x -> $x op e@.
--   * @(e op)@ — left-section, desugars to @\\$x -> e op $x@.
--
-- `openTok` is the @(@ token; `cur` is positioned just after it.
parseParenExpr :: Ctx -> Token -> Cursor -> IO (Expr, Cursor)
parseParenExpr ctx _openTok cur0 = do
    let (peek, curP) = nextSig ctx cur0
    case tkKind peek of
        TkRParen -> pure (EVar "()", curP)
        -- Operator-as-value, optionally followed by right-operand (section).
        _ | Just opName <- tokenOpName (tkKind peek)
          , tkKind peek /= TkMinus        -- `(-1)` is NEG 1, not a section
          -> do
            let (afterOp, curAfterOp) = nextSig ctx curP
            case tkKind afterOp of
                TkRParen -> pure (EVar opName, curAfterOp)
                _ -> do
                    -- Right section: (op rhs) = \$x -> $x op rhs
                    (rhs, curR) <- parseExpr ctx curP
                    let (closeTok, curC) = nextSig ctx curR
                    case tkKind closeTok of
                        TkRParen ->
                            let n = "$s"
                                body = EApp (EApp (EVar opName) (EVar n)) rhs
                            in pure (ELam n body, curC)
                        _ -> parseErr "expected `)` in section" closeTok
          | tkKind peek == TkBacktick -> do
            -- Backtick section: (`f` e) = \$x -> f $x e
            let (idTok, curId) = nextSig ctx curP
            case tkKind idTok of
                TkIdent fn -> do
                    let (bt2, curB2) = nextSig ctx curId
                    case tkKind bt2 of
                        TkBacktick -> do
                            let (afterOp, curAfterOp) = nextSig ctx curB2
                            case tkKind afterOp of
                                TkRParen -> pure (EVar fn, curAfterOp)
                                _ -> do
                                    (rhs, curR) <- parseExpr ctx curB2
                                    let (closeTok, curC) = nextSig ctx curR
                                    case tkKind closeTok of
                                        TkRParen ->
                                            let n = "$s"
                                                body = EApp (EApp (EVar fn) (EVar n)) rhs
                                            in pure (ELam n body, curC)
                                        _ -> parseErr "expected `)` in backtick section" closeTok
                        _ -> parseErr "expected closing backtick in section" bt2
                _ -> parseErr "expected identifier in backtick section" idTok
        _ -> do
            -- Fully general expression, possibly followed by:
            --   - `)`  → single expression, or unit.
            --   - `,`  → tuple, collect more.
            --   - `::` → type annotation, swallow to matching `)`.
            --   - an operator + `)` → left section.
            (e, cur1) <- parseExpr ctx cur0
            let (sep, cur2) = nextSig ctx cur1
            case tkKind sep of
                TkRParen -> pure (e, cur2)
                TkComma  -> do
                    (rest, curEnd) <- gatherTupleExpr ctx cur2 []
                    pure (ETuple (e : rest), curEnd)
                TkDColon -> do
                    curEnd <- skipTypeToClose ctx cur2
                    pure (e, curEnd)
                _ | Just opName <- tokenOpName (tkKind sep) -> do
                    -- Left section: (e op) = \$x -> e op $x
                    let (afterOp, curAfterOp) = nextSig ctx cur2
                    case tkKind afterOp of
                        TkRParen ->
                            let n = "$s"
                                body = EApp (EApp (EVar opName) e) (EVar n)
                            in pure (ELam n body, curAfterOp)
                        _ -> parseErr "expected `)` after operator in section" afterOp
                  | tkKind sep == TkBacktick -> do
                    -- Left backtick section: (e `f`)
                    let (idTok, curId) = nextSig ctx cur2
                    case tkKind idTok of
                        TkIdent fn -> do
                            let (bt2, curB2) = nextSig ctx curId
                            case tkKind bt2 of
                                TkBacktick -> do
                                    let (afterOp, curAfterOp) = nextSig ctx curB2
                                    case tkKind afterOp of
                                        TkRParen ->
                                            let n = "$s"
                                                body = EApp (EApp (EVar fn) e) (EVar n)
                                            in pure (ELam n body, curAfterOp)
                                        _ -> parseErr "expected `)` after left backtick section" afterOp
                                _ -> parseErr "expected closing backtick in left section" bt2
                        _ -> parseErr "expected identifier in backtick section" idTok
                _ -> parseErr "expected `)` or `,` in parenthesised expression" sep

-- | Collect more tuple components after the first @,@. Starts just after
-- a comma.
gatherTupleExpr :: Ctx -> Cursor -> [Expr] -> IO ([Expr], Cursor)
gatherTupleExpr ctx cur acc = do
    (e, cur1) <- parseExpr ctx cur
    let (sep, cur2) = nextSig ctx cur1
    case tkKind sep of
        TkComma  -> gatherTupleExpr ctx cur2 (e : acc)
        TkRParen -> pure (reverse (e : acc), cur2)
        _        -> parseErr "expected `,` or `)` in tuple" sep

parseUnboxedTuple :: Ctx -> Cursor -> IO (Expr, Cursor)
parseUnboxedTuple ctx cur0 = do
    -- `#)` is NOT lexed as a single token (that would misread string-lit
    -- primops like @"x"#)@), so we accept either 'TkRUnbox' or the pair
    -- @TkSymOp "#" ; TkRParen@ as the closing delimiter.
    --
    -- (# #) → unit, (# e #) → e (single-element), (# e1, e2, ... #) →
    -- EApp chain using a named unboxed-tuple constructor "(#,#)" / "(#,,#)" etc.
    if isUnboxClose ctx cur0
        then pure (EVar "()", skipUnboxClose ctx cur0)
        else do
            (e, cur1) <- parseExpr ctx cur0
            if isUnboxClose ctx cur1
                then pure (e, skipUnboxClose ctx cur1)
                else do
                    let (sep, cur2) = nextSig ctx cur1
                    case tkKind sep of
                        TkComma -> do
                            (rest, curEnd) <- gatherUnboxed ctx cur2 []
                            let elems = e : rest
                                arity = length elems
                                conName = BC.pack ("(#" <> replicate (arity - 1) ',' <> "#)")
                                -- Build EApp chain: (((EVar conName) e1) e2) ...
                                con  = EVar conName
                                expr = foldl EApp con elems
                            pure (expr, curEnd)
                        _ -> parseErr "expected `,` or `#)` in unboxed tuple" sep
  where
    gatherUnboxed c cur acc = do
        (e, cur1) <- parseExpr c cur
        if isUnboxClose c cur1
            then pure (reverse (e : acc), skipUnboxClose c cur1)
            else do
                let (sep, cur2) = nextSig c cur1
                case tkKind sep of
                    TkComma -> gatherUnboxed c cur2 (e : acc)
                    _       -> parseErr "expected `,` or `#)` in unboxed tuple" sep

-- | Is the next token sequence an unboxed-tuple close: @#)@?
isUnboxClose :: Ctx -> Cursor -> Bool
isUnboxClose ctx cur =
    let (t1, cur1) = nextSig ctx cur in
    case tkKind t1 of
        TkRUnbox -> True
        TkSymOp op | op == BC.pack "#" ->
            let (t2, _) = nextSig ctx cur1 in
            tkKind t2 == TkRParen
        _ -> False

skipUnboxClose :: Ctx -> Cursor -> Cursor
skipUnboxClose ctx cur =
    let (t1, cur1) = nextSig ctx cur in
    case tkKind t1 of
        TkRUnbox -> cur1
        TkSymOp _ ->
            let (_, cur2) = nextSig ctx cur1 in cur2
        _ -> cur1

-- | After consuming `::` inside a parenthesised expression, skip tokens
-- at paren-depth 0 until we see the closing `)`. Arbitrary type-level
-- tokens are tolerated (forall, =>, #, unboxed tuples, brackets).
skipTypeToClose :: Ctx -> Cursor -> IO Cursor
skipTypeToClose ctx cur0 = go cur0 (0 :: Int)
  where
    go cur depth = do
        let (tok, cur') = nextSig ctx cur
        case tkKind tok of
            TkEof                 -> pure cur'
            TkRParen | depth == 0 -> pure cur'
            TkLParen              -> go cur' (depth + 1)
            TkRParen              -> go cur' (depth - 1)
            TkLBracket            -> go cur' (depth + 1)
            TkRBracket            -> go cur' (depth - 1)
            TkLUnbox              -> go cur' (depth + 1)
            TkRUnbox              -> go cur' (depth - 1)
            _                     -> go cur' depth

--------------------------------------------------------------------------------
-- Qualified-name fusion (unchanged from Phase 2.5)
--------------------------------------------------------------------------------

tryQualified :: Ctx -> Name -> Token -> Cursor -> IO (Expr, Cursor)
tryQualified ctx firstName firstTok cur0 =
    go firstName firstTok cur0
  where
    src = ctxSrc ctx
    go acc lastTok cur =
        let (dotTok, curAfterDot) = nextToken src cur in
        case tkKind dotTok of
            TkDot | tkStart dotTok == tkEnd lastTok ->
                let (segTok, curAfterSeg) = nextToken src curAfterDot in
                case tkKind segTok of
                    TkIdent n | tkStart segTok == tkEnd dotTok ->
                        pure (EVar (acc <> BC.pack "." <> n), curAfterSeg)
                    TkConId n | tkStart segTok == tkEnd dotTok ->
                        go (acc <> BC.pack "." <> n) segTok curAfterSeg
                    _ -> pure (EVar acc, cur)
            _ -> pure (EVar acc, cur)

--------------------------------------------------------------------------------
-- String literal desugar + list literals
--------------------------------------------------------------------------------

stringToConsList :: String -> Expr
stringToConsList []     = EVar "[]"
stringToConsList (c:cs) = EApp (EApp (EVar ":") (ELit (LChar c))) (stringToConsList cs)

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
            -- List comprehension @[ e | q1, q2, ... ]@ — Phase 2.6 parses
            -- the shape but doesn't evaluate it; we swallow through the
            -- closing @]@ and hand back a singleton list @[e]@ so
            -- higher-level code continues to parse.
            TkBar -> do
                curEnd <- skipToCloseBracket ctx cur1
                pure (buildCons (reverse acc), curEnd)
            -- Range expression @[a..b]@ or @[a,b..c]@ — approximate by
            -- returning a singleton of the first element so downstream
            -- code still parses. Full range evaluation is deferred.
            TkDotDot -> do
                curEnd <- skipToCloseBracket ctx cur1
                pure (buildCons (reverse acc), curEnd)
            _ -> parseErr "expected `,` or `]` in list literal" tok

    buildCons :: [Expr] -> Expr
    buildCons []     = EVar "[]"
    buildCons (e:es) = EApp (EApp (EVar ":") e) (buildCons es)

-- | Fast-forward through a balanced bracket group, returning the cursor
-- just past the matching @]@. Used only for list-comprehensions and
-- ranges that we don't yet understand but want to accept syntactically.
-- Tracks nested @[@…@]@, @(@…@)@, and @{@…@}@ so stray delimiters inside
-- the comprehension body don't confuse the skipper.
skipToCloseBracket :: Ctx -> Cursor -> IO Cursor
skipToCloseBracket ctx cur0 = go cur0 (0 :: Int) (0 :: Int) (0 :: Int)
  where
    go cur bDepth pDepth cDepth = do
        let (tok, cur') = nextSig ctx cur
        case tkKind tok of
            TkEof      -> pure cur'
            TkRBracket | bDepth == 0 && pDepth == 0 && cDepth == 0 -> pure cur'
            TkLBracket -> go cur' (bDepth + 1) pDepth cDepth
            TkRBracket -> go cur' (bDepth - 1) pDepth cDepth
            TkLParen   -> go cur' bDepth (pDepth + 1) cDepth
            TkRParen   -> go cur' bDepth (pDepth - 1) cDepth
            TkLBrace   -> go cur' bDepth pDepth (cDepth + 1)
            TkRBrace   -> go cur' bDepth pDepth (cDepth - 1)
            _          -> go cur' bDepth pDepth cDepth

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

parseErr :: String -> Token -> IO a
parseErr msg tok =
    throwIO (ParseError (msg <> " at offset " <> show (tkStart tok)
                         <> " but saw " <> show (tkKind tok)))
