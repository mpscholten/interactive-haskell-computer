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

import Control.Exception (Exception(..), throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import IHC.AST
import IHC.Lexer
import IHC.Scan (Clause(..))
import IHC.Source

-- | A parse error carrying the source location so the error handler can
-- print @file:line:col@ instead of a raw byte offset.
data ParseError = ParseError
    { peFile :: !FilePath
    , peLine :: !Int
    , peCol  :: !Int
    , peMsg  :: !String
    } deriving (Show)

instance Exception ParseError where
    displayException (ParseError file line col msg) =
        "parse error at " <> file <> ":" <> show line <> ":" <> show col
        <> "\n  " <> msg

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
parseBodyExprWithFixity src _ [] = throwIO (ParseError
    { peFile = srcName src, peLine = 0, peCol = 0
    , peMsg  = "empty clause list" })
parseBodyExprWithFixity src fx clauses = do
    parsed <- mapM (parseClause src fx) clauses
    let arity = case parsed of
            ((ps, _) : _) -> length ps
            _             -> 0
    mapM_ (\(ps, _) ->
                if length ps == arity
                    then pure ()
                    else throwIO (ParseError
                        { peFile = srcName src, peLine = 0, peCol = 0
                        , peMsg  = "clauses have differing arities" }))
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
            RhsGuards ges -> RhsGuards [(wrapWhere g, wrapWhere b) | (g, b) <- ges]
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
        let (tok, cur1) = nextSig ctx cur
        case tkKind tok of
            TkEof -> pure (reverse acc)
            -- Infix LHS form: `arg1 \`funcName\` arg2 = body`
            -- The backtick + function name + backtick appear between the
            -- two argument patterns and must be skipped (not parsed as a
            -- pattern).  After skipping, continue collecting patterns.
            TkBacktick -> do
                case readBacktickName ctx cur1 of
                    Just (_, cur2) -> do
                        -- Consume the closing backtick and continue.
                        let (closeTok, cur3) = nextSig ctx cur2
                        case tkKind closeTok of
                            TkBacktick -> loop ctx cur3 acc
                            _          -> pure (reverse acc)
                    Nothing -> pure (reverse acc)
            -- Operator infix LHS form: `arg1 OP arg2 = body`
            -- The symbolic operator (e.g. @?=, <>) between the two
            -- argument patterns must be skipped.
            TkSymOp _ -> loop ctx cur1 acc
            -- '@'-prefixed operator infix LHS: arg1 @?= arg2 = body
            -- '@' is TkAt (not isOpChar), followed by TkSymOp for the rest.
            -- Skip both tokens and continue collecting patterns.
            TkAt ->
                let (peek2, cur2) = nextSig ctx cur1
                in case tkKind peek2 of
                    TkSymOp _ -> loop ctx cur2 acc
                    _         -> loop ctx cur1 acc
            _ | not (startsPat (tkKind tok)) -> pure (reverse acc)
              | otherwise -> do
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
        _ -> parseErr ctx "expected `=` or `|` at start of RHS" firstTok
  where
    parseGuards ctx cur acc = do
        (g, cur1) <- parseExpr ctx cur
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` after guard" eqTok
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
        -- ViewPatterns: (f -> p) desugars to
        --   let $vpN = f argName in case $vpN of { p -> restBody; _ -> fallback }
        PView fn vp ->
            let n = BC.pack ("$vp" <> BC.unpack argName)
                vpBody = matchPatterns rest body fallback
            in ELet [(n, EApp fn (EVar argName))]
                   (ECase (EVar n)
                       [ Alt vp vpBody
                       , Alt PWild fallback
                       ])
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
    -- Parse one clause of a named binding (name + params + rhs).
    -- Returns (name, params, Rhs, cursor-after).
    -- Rhs is either RhsPlain (= expr) or RhsGuards (| g = e ...).
    parseClauseRaw ctx cur = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkIdent n -> pure n
            _         -> parseErr ctx "expected identifier in binding; saw" nameTok
        -- Allow LHS parameters before `=` or `|`, e.g. `f x y = body`.
        (params, cur2) <- collectLetParams ctx cur1 []
        let (sepTok, cur3) = nextSig ctx cur2
        case tkKind sepTok of
            TkEq -> do
                (expr, cur4) <- parseExpr ctx cur3
                pure (name, params, RhsPlain expr, cur4)
            TkBar -> do
                (branches, cur4) <- parseLetGuardBranches ctx cur3 []
                pure (name, params, RhsGuards branches, cur4)
            _ -> parseErr ctx "expected `=` or `|` in binding" sepTok

    -- | Collect all variable names bound by a pattern (left-to-right order).
    patVars (PVar n)         = [n]
    patVars (PAs n p)        = n : patVars p
    patVars (PBang p)        = patVars p
    patVars (PTuple ps)      = concatMap patVars ps
    patVars (PCon _ ps)      = concatMap patVars ps
    patVars (PRecord _ fps)  = concatMap (patVars . snd) fps
    patVars (PRecordWild _)  = []   -- wildcard record: bound names are implicit, skip
    patVars (PView _ p)      = patVars p
    patVars PWild            = []
    patVars (PLit _)         = []

    -- | Parse a where-block pattern binding.
    -- Returns (tmpBind, perVarBinds, curAfter).
    -- e.g. (a, b) = rhs  =>
    --   $wh0 = rhs
    --   a    = case $wh0 of { (a, _) -> a }
    --   b    = case $wh0 of { (_, b) -> b }
    parseWherePatBind ctx accLen cur = do
        (pat, cur2) <- parseSubPat ctx cur
        let (eqTok, cur3) = nextSig ctx cur2
        case tkKind eqTok of
            TkEq -> do
                (rhsE, cur4) <- parseExpr ctx cur3
                let tmpName  = BC.pack ("$wh" ++ show accLen)
                    tmpBind  = (tmpName, rhsE)
                    vars     = patVars pat
                    -- For each bound variable v, produce:
                    --   v = case $tmp of { pat -> v }
                    -- where pat is the original pattern (variables other than v
                    -- are still bound by the same match; we just project v out).
                    perVarBind v =
                        ( v
                        , ECase (EVar tmpName)
                            [ Alt pat (EVar v)
                            , Alt PWild (EApp (EVar "error")
                                          (stringToConsList
                                            "Non-exhaustive pattern in where"))
                            ]
                        )
                    newBinds = tmpBind : map perVarBind vars
                pure (newBinds, cur4)
            _ -> parseErr ctx "expected `=` in where-block pattern binding" eqTok

    -- Parse one or more `| guard = expr` branches for let/where bindings.
    parseLetGuardBranches ctx cur acc = do
        (g, cur1) <- parseExpr ctx cur
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` after guard expression" eqTok
        (b, cur3) <- parseExpr ctx cur2
        let acc' = acc ++ [(g, b)]
        let (sep, cur4) = nextSig ctx cur3
        case tkKind sep of
            TkBar -> parseLetGuardBranches ctx cur4 acc'
            _     -> pure (acc', cur3)

    -- Parse a single binding at the current column, collecting all
    -- consecutive same-name clauses so multi-clause where-bindings
    -- (e.g. `f _ [] = ...; f x (y:_) = ...`) are properly desugared.
    -- Returns a list of Bind because a pattern binding expands to multiple binds.
    parseOne ctx bindCol accLen cur = do
        let (peekTok, _) = nextSig ctx cur
        case tkKind peekTok of
            TkIdent _ -> do
                -- Normal named binding (identifier starts the binding).
                (name, params0, rhs0, cur1) <- parseClauseRaw ctx cur
                let clause0 = (params0, rhs0)
                -- Peek: if the next binding-column token has the same name,
                -- it is another clause of the same definition. Collect all of them.
                (moreClauses, curFinal) <- collectMoreWhereClauses ctx bindCol name cur1 []
                let allClauses = clause0 : moreClauses
                    arity      = length params0
                -- Desugar multi-clause or single-clause into a single Expr.
                let expr = desugarClauses allClauses arity
                pure ([(name, expr)], curFinal)
            _ | startsPat (tkKind peekTok) -> do
                -- Pattern binding: (a, b) = rhs, Con x = rhs, etc.
                (newBinds, cur') <- parseWherePatBind ctx accLen cur
                pure (newBinds, cur')
              | otherwise ->
                parseErr ctx "expected identifier or pattern in where-binding" peekTok

    -- Collect additional same-name clauses (for multi-clause where-bindings).
    collectMoreWhereClauses ctx bindCol name cur acc = do
        let (peekTok, _) = nextSig ctx cur
        case tkKind peekTok of
            TkIdent n | n == name && tkCol peekTok == bindCol -> do
                (_, params, rhs, cur') <- parseClauseRaw ctx cur
                collectMoreWhereClauses ctx bindCol name cur'
                    ((params, rhs) : acc)
            _ -> pure (reverse acc, cur)

    -- | Try to skip a type signature at the current position.
    -- Returns @Just curAfter@ if a @name[, name]* :: type@ sig was found
    -- and skipped, or @Nothing@ if this is a regular value binding.
    -- @bindCol@ is the binding column; the type body ends when a token
    -- at column @<= bindCol@ is seen.
    trySkipWhereSig ctx bindCol cur = do
        let (tok1, cur1) = nextSig ctx cur
        case tkKind tok1 of
            TkIdent _ ->
                -- Could be start of a sig: peek for ',' or '::'
                let skipNames c = do
                        let (t, c') = nextSig ctx c
                        case tkKind t of
                            TkComma ->
                                -- multi-name sig: a, b, c :: T
                                let (t2, c2) = nextSig ctx c' in
                                case tkKind t2 of
                                    TkIdent _ -> skipNames c2
                                    _         -> pure Nothing  -- parse error, fall through
                            TkDColon -> do
                                curAfter <- skipTypeSigBody ctx bindCol c'
                                pure (Just curAfter)
                            _ -> pure Nothing   -- not a sig, let parseOne handle it
                in skipNames cur1
            _ -> pure Nothing

    braced ctx cur acc
        | cPos cur >= ctxEnd ctx = pure (reverse acc)
        | otherwise = do
            let (nameTok, _) = nextSig ctx cur
            -- Peek the bind column from the current position.
            let bindCol = tkCol nameTok
            mSkip <- trySkipWhereSig ctx bindCol cur
            case mSkip of
                Just cur' -> do
                    let (nextTok, _) = nextSig ctx cur'
                    case tkKind nextTok of
                        TkSemi   -> braced ctx cur' acc
                        TkRBrace -> pure (reverse acc)
                        TkEof    -> pure (reverse acc)
                        _ | cPos cur' < ctxEnd ctx -> braced ctx cur' acc
                          | otherwise -> pure (reverse acc)
                Nothing -> do
                    (bs, cur') <- parseOne ctx bindCol (length acc) cur
                    let acc' = reverse bs ++ acc
                    let (sep, curN) = nextSig ctx cur'
                    case tkKind sep of
                        TkSemi   -> braced ctx curN acc'
                        TkRBrace -> pure (reverse acc')
                        TkEof    -> pure (reverse acc')
                        _        -> parseErr ctx "expected `;` or `}` in let/where" sep

    layout ctx bindCol cur acc
        | cPos cur >= ctxEnd ctx = pure (reverse acc)
        | otherwise = do
            mSkip <- trySkipWhereSig ctx bindCol cur
            case mSkip of
                Just cur' -> do
                    -- Type sig skipped; continue if next token is at same col.
                    let (nextTok, _) = nextSig ctx cur'
                    case tkKind nextTok of
                        TkEof -> pure (reverse acc)
                        _ | tkCol nextTok == bindCol && cPos cur' < ctxEnd ctx ->
                               layout ctx bindCol cur' acc
                          | otherwise ->
                               pure (reverse acc)
                Nothing -> do
                    (bs, cur') <- parseOne ctx bindCol (length acc) cur
                    let acc' = reverse bs ++ acc
                    let (nextTok, _) = nextSig ctx cur'
                    case tkKind nextTok of
                        TkEof -> pure (reverse acc')
                        _ | tkCol nextTok == bindCol && cPos cur' < ctxEnd ctx ->
                               layout ctx bindCol cur' acc'
                          | otherwise ->
                               pure (reverse acc')

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

-- | Skip the body of a @name :: type@ signature inside a where/let block.
-- Called after consuming @::@. Stops when it finds a token whose column is
-- @<= bindCol@ at bracket depth 0, or at EOF. This is column-aware rather
-- than stopping at @=@, so it handles multi-token type expressions
-- (@Int -> Int@, @Maybe (Foo -> Bar)@, etc.) correctly.
skipTypeSigBody :: Ctx -> Int -> Cursor -> IO Cursor
skipTypeSigBody ctx bindCol cur0 = go cur0 (0 :: Int) (0 :: Int) (0 :: Int)
  where
    go cur b p c = do
        let (tok, cur') = nextSig ctx cur
        case tkKind tok of
            TkEof -> pure cur
            _ | tkCol tok <= bindCol && b == 0 && p == 0 && c == 0 -> pure cur
            TkRParen | p > 0    -> go cur' b (p - 1) c
            TkRParen            -> pure cur  -- unmatched ')' — stop before it
            TkRBracket | b > 0  -> go cur' (b - 1) p c
            TkRBracket          -> pure cur
            TkRBrace | c > 0    -> go cur' b p (c - 1)
            TkRBrace            -> pure cur
            TkLParen   -> go cur' b (p + 1) c
            TkLBracket -> go cur' (b + 1) p c
            TkLBrace   -> go cur' b p (c + 1)
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
            _      -> parseErr ctx "expected `then`" t1
        (th, cur3) <- parseExpr c2 cur2
        let (t2, cur4) = nextSig c2 cur3
        case tkKind t2 of
            TkElse -> pure ()
            _      -> parseErr ctx "expected `else`" t2
        (el, cur5) <- parseExpr c2 cur4
        pure (EIf cond th el, cur5)

    parseMultiWayIf c2 cur acc = do
        (g, cur1) <- parseExpr c2 cur
        let (arr, cur2) = nextSig c2 cur1
        case tkKind arr of
            TkArrow -> pure ()
            _       -> parseErr ctx "expected `->` in multi-way if" arr
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
                then parseErr ctx "expected at least one pattern in lambda" tok
                else do
                    let (_, curAfter) = nextSig ctx cur
                    pure (reverse acc, curAfter)
        _ | startsPat (tkKind tok) -> do
            (p, cur') <- parseSubPat ctx cur
            collectLamPats ctx cur' (p : acc)
          | otherwise ->
            parseErr ctx "expected pattern or `->` in lambda" tok

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

-- Note: {-# LANGUAGE ApplicativeDo #-} is accepted (the lexer silently skips
-- all pragmas).  Regardless of the pragma, we always try the applicative
-- desugaring when a do-block's binds are independent — monadic fallback is
-- semantically equivalent (Applicative is a superclass of Monad), and the
-- applicative form signals pipelineable structure to backends that care
-- (e.g. hasql's generated row decoders, IHP.FetchPipelined).
--
-- The applicative transform fires when all of the following hold:
--
--   1. Every non-final statement is an 'SBind' (no middle 'SLet' or 'SExpr').
--   2. The final statement is @SExpr (pure e)@ or @SExpr (return e)@.
--   3. No bind's RHS references a name bound by an earlier bind.
--
-- Under those conditions,
--   do { x1 <- a1; ... ; xn <- an; pure e }
-- becomes
--   (\x1 ... xn -> e) <$> a1 <*> a2 <*> ... <*> an
--
-- Anything that doesn't match the above falls back to the standard monadic
-- >>= / >> chain.

parseDo :: Ctx -> Cursor -> IO (Expr, Cursor)
parseDo ctx cur0 = do
    let (firstTok, curAfter) = nextSig ctx cur0
    case tkKind firstTok of
        TkLBrace -> do
            stmts <- bracedStmts curAfter []
            case stmts of
                (ss, cur') -> pure (desugarDo ss, cur')
        TkEof    -> pure (EDo [], cur0)
        _        -> do
            stmts <- layoutStmts (tkCol firstTok) cur0 []
            case stmts of
                (ss, cur') -> pure (desugarDo ss, cur')
  where
    bracedStmts cur acc = do
        (s, cur') <- parseStmt ctx cur
        let (sep, curN) = nextSig ctx cur'
        case tkKind sep of
            TkSemi   -> bracedStmts curN (s : acc)
            TkRBrace -> pure (reverse (s : acc), curN)
            _        -> parseErr ctx "expected `;` or `}` in do-block" sep

    layoutStmts stmtCol cur acc = do
        let stmtCtx = ctx { ctxMinCol = stmtCol }
        (s, cur') <- parseStmt stmtCtx cur
        let (nextTok, _) = nextSig ctx cur'
        case tkKind nextTok of
            TkEof -> pure (reverse (s : acc), cur')
            -- `where`, `in`, `of`, `then`, `else` are block-ending keywords:
            -- they cannot be the start of a do-statement even when they happen
            -- to appear at the statement column.  This arises in code like:
            --   action = do
            --     stmt1
            --     stmt2
            --   where
            --     helper = ...
            -- where `where` sits at the same indentation level as the stmts.
            TkWhere | tkCol nextTok == stmtCol -> pure (reverse (s : acc), cur')
            TkIn    | tkCol nextTok == stmtCol -> pure (reverse (s : acc), cur')
            _ | tkCol nextTok == stmtCol -> layoutStmts stmtCol cur' (s : acc)
              | otherwise                -> pure (reverse (s : acc), cur')

    -- | Desugar a list of do-statements.  First try the applicative form;
    -- if that doesn't apply, fall back to the classical monadic chain.
    desugarDo :: [Stmt] -> Expr
    desugarDo ss
      | Just appl <- tryApplicativeDo ss = appl
      | otherwise                        = monadicDo ss

    -- | Standard Haskell do-notation desugaring using >>=/>>/let.
    monadicDo :: [Stmt] -> Expr
    monadicDo []               = EDo []  -- shouldn't happen; fallback
    monadicDo [SExpr e]        = e
    monadicDo [SBind _ e]      = e  -- last stmt can't be bind, but be defensive
    monadicDo [SLet bs]        = ELet bs (EDo [])  -- shouldn't happen
    monadicDo (SExpr e : rest) =
        -- e >> do { rest }
        EApp (EApp (EVar ">>") e) (monadicDo rest)
    monadicDo (SBind name e : rest) =
        -- e >>= \name -> do { rest }
        EApp (EApp (EVar ">>=") e) (ELam name (monadicDo rest))
    monadicDo (SLet bs : rest) =
        -- let bs in do { rest }
        ELet bs (monadicDo rest)

    -- | If the do-block matches the applicative pattern, return its
    -- applicative desugaring.  See the header comment on 'parseDo' for the
    -- full set of conditions.
    tryApplicativeDo :: [Stmt] -> Maybe Expr
    tryApplicativeDo stmts = do
        -- Need at least one SBind plus a final SExpr (pure e).  A single
        -- bind still benefits: @do { x <- a; pure e }@ becomes
        -- @fmap (\\x -> e) a@, which is a cheap win over @a >>= \\x -> pure e@.
        (binds, finalExpr) <- splitBindsAndPure stmts
        case binds of
            []      -> Nothing          -- no binds → nothing to parallelize
            _       -> buildAppl binds finalExpr
      where
        -- | Expect @[SBind n1 a1, ..., SBind nk ak, SExpr (pure e)]@ and
        -- return @(binds, e)@ on success.  Any middle 'SExpr'/'SLet', or a
        -- final statement that isn't literally @pure …@ / @return …@, fails.
        splitBindsAndPure :: [Stmt] -> Maybe ([(Name, Expr)], Expr)
        splitBindsAndPure []             = Nothing
        splitBindsAndPure [SExpr final]  = do
            inner <- pureBody final
            pure ([], inner)
        splitBindsAndPure (SBind n e : rest) = do
            (bs, inner) <- splitBindsAndPure rest
            pure ((n, e) : bs, inner)
        splitBindsAndPure _              = Nothing

        -- | Detect a final expression of the form @pure e@ or @return e@
        -- (unqualified only — qualified Prelude.pure is rare and easy to
        -- conservatively miss).
        pureBody :: Expr -> Maybe Expr
        pureBody (EApp (EVar "pure")   e) = Just e
        pureBody (EApp (EVar "return") e) = Just e
        pureBody _                        = Nothing

        -- | Check independence: for @binds = [(n1,a1), …, (nk,ak)]@, no
        -- @ai@ may reference any @nj@ with j < i.  If that holds, build
        -- @(\\n1 … nk -> body) <$> a1 <*> a2 <*> … <*> ak@.
        buildAppl :: [(Name, Expr)] -> Expr -> Maybe Expr
        buildAppl binds body
            | not (independent [] binds) = Nothing
            -- Emit @fmap (\\x1 … xk -> e) a1 \<*\> a2 \<*\> … \<*\> ak@.
            -- Using 'fmap' rather than @\<\$\>@ avoids pulling in the
            -- @Data.Functor@ import when the user's module didn't already
            -- do so; 'fmap' is a known builtin in 'IHC.Builtins'.
            | (a : as) <- map snd binds =
                let names    = map fst binds
                    lam      = foldr ELam body names
                    fmapPart = EApp (EApp (EVar "fmap") lam) a
                    stepAp acc rhs = EApp (EApp (EVar "<*>") acc) rhs
                in Just (foldl stepAp fmapPart as)
            | otherwise = Nothing    -- empty binds caught by caller
          where
            -- Each bind's RHS is checked against previously-bound names,
            -- then that bind's own name is added to the seen set for
            -- subsequent binds.
            independent _    []             = True
            independent seen ((n, rhs) : rs) =
                all (`notElem` seen) (exprFreeVars rhs)
                && independent (n : seen) rs

    -- | Free variables of an 'Expr' (names referenced via 'EVar' that
    -- aren't shadowed by a lambda, let, or pattern binding within).  Kept
    -- local so the parser doesn't depend on the scheduler; the version in
    -- 'IHC.Scheduler' covers more constructors but this subset is enough
    -- for ApplicativeDo's independence check.
    exprFreeVars :: Expr -> [Name]
    exprFreeVars = fv []
      where
        fv bound = \case
            EVar n
                | n `elem` bound -> []
                | otherwise      -> [n]
            ELit _      -> []
            EApp f x    -> fv bound f ++ fv bound x
            ELam n e    -> fv (n : bound) e
            ELet bs e   ->
                let names = map fst bs
                    bound' = names ++ bound
                in concatMap (fv bound' . snd) bs ++ fv bound' e
            ECase s as  -> fv bound s ++ concatMap (fvAlt bound) as
            EIf c t e   -> fv bound c ++ fv bound t ++ fv bound e
            EDo ss      -> fvStmts bound ss
            ENeg e      -> fv bound e
            ETuple es   -> concatMap (fv bound) es
            ERecordCon    _ fs -> concatMap (fv bound . snd) fs
            ERecordWild   _    -> []
            ERecordUpdate e fs -> fv bound e ++ concatMap (fv bound . snd) fs
            EImplicitRef  _    -> []
            EImplicitLet bs e  ->
                let names = map fst bs
                    bound' = names ++ bound
                in concatMap (fv bound' . snd) bs ++ fv bound' e
            ESplice inner      -> fv bound inner
            EQuote  _          -> []
            ELabel  _          -> []

        fvStmts _     []                  = []
        fvStmts bound (SExpr e   : rest)  = fv bound e ++ fvStmts bound rest
        fvStmts bound (SBind n e : rest)  = fv bound e ++ fvStmts (n : bound) rest
        fvStmts bound (SLet bs   : rest)  =
            let names  = map fst bs
                bound' = names ++ bound
            in concatMap (fv bound' . snd) bs ++ fvStmts bound' rest

        fvAlt bound (Alt p e) = fv (patBound p ++ bound) e

        patBound :: Pat -> [Name]
        patBound (PVar n)        = [n]
        patBound (PCon _ ps)     = concatMap patBound ps
        patBound (PAs n p)       = n : patBound p
        patBound (PBang p)       = patBound p
        patBound (PTuple ps)     = concatMap patBound ps
        patBound (PRecord _ fps) = concatMap (patBound . snd) fps
        patBound (PRecordWild _) = []
        patBound (PView _ p)     = patBound p
        patBound _               = []

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
        -- `_ <- expr` — wildcard bind; discard the result.
        TkUnderscore -> do
            let (peek, cur2) = nextSig ctx cur1
            case tkKind peek of
                TkLArrow -> do
                    (e, cur3) <- parseExpr ctx cur2
                    pure (SBind (BC.pack "_") e, cur3)
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
        _        ->
            -- Layout mode: collect all bindings at the same column.
            -- This handles `let x :: Int\n    x = 42` correctly.
            let bindCol = tkCol firstTok
            in layoutBinds bindCol cur0 []
    pure (SLet binds, curEnd)
  where
    -- | Try to detect and skip a type sig @name[, name]* :: type@ at the
    -- current position. Returns @Just curAfter@ on success, @Nothing@ if
    -- this looks like a value binding.
    trySkipDoLetSig bindCol cur = do
        let (tok1, cur1) = nextSig ctx cur
        case tkKind tok1 of
            TkIdent _ ->
                let skipNames c = do
                        let (t, c') = nextSig ctx c
                        case tkKind t of
                            TkComma ->
                                let (t2, c2) = nextSig ctx c' in
                                case tkKind t2 of
                                    TkIdent _ -> skipNames c2
                                    _         -> pure Nothing
                            TkDColon -> do
                                curAfter <- skipTypeSigBody ctx bindCol c'
                                pure (Just curAfter)
                            _ -> pure Nothing
                in skipNames cur1
            _ -> pure Nothing

    -- Layout-mode: collect one or more bindings at `bindCol`, skipping type sigs.
    -- Stops when the next token is at a different column or EOF.
    layoutBinds bindCol cur acc = do
        mSkip <- trySkipDoLetSig bindCol cur
        case mSkip of
            Just cur' -> do
                let (peek, _) = nextSig ctx cur'
                if tkCol peek == bindCol && tkKind peek /= TkEof
                    then layoutBinds bindCol cur' acc
                    else pure (reverse acc, cur')
            Nothing -> do
                let (nameTok, cur1) = nextSig ctx cur
                name <- case tkKind nameTok of
                    TkIdent n -> pure n
                    _         -> parseErr ctx "expected identifier after `let`" nameTok
                (params, cur2) <- collectLetParams ctx cur1 []
                let (eqTok, cur3) = nextSig ctx cur2
                case tkKind eqTok of
                    TkEq -> pure ()
                    _    -> parseErr ctx "expected `=` in let-binding" eqTok
                (e, cur4) <- parseExpr ctx cur3
                let bind = (name, wrapParams params e)
                let (peek, _) = nextSig ctx cur4
                if tkCol peek == bindCol && tkKind peek /= TkEof
                    then layoutBinds bindCol cur4 (bind : acc)
                    else pure (reverse (bind : acc), cur4)

    bracedBinds cur acc = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkIdent n -> pure n
            _         -> parseErr ctx "expected identifier in let-binding" nameTok
        (params, cur2) <- collectLetParams ctx cur1 []
        let (eqTok, cur3) = nextSig ctx cur2
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` in let-binding" eqTok
        (e, cur4) <- parseExpr ctx cur3
        let (sep, curN) = nextSig ctx cur4
        case tkKind sep of
            TkSemi   -> bracedBinds curN ((name, wrapParams params e) : acc)
            TkRBrace -> pure (reverse ((name, wrapParams params e) : acc), curN)
            _        -> parseErr ctx "expected `;` or `}` in let-block" sep

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
    -- Phase 3.6: peek whether this is an implicit-param let (?x = ...).
    let (firstTok, curAfter) = nextSig ctx cur0
    case tkKind firstTok of
        TkImplicitRef _ -> parseImplicitLet ctx cur0
        TkLBrace -> do
            -- Could be braced implicit or regular. Peek inside.
            let (peek, _) = nextSig ctx curAfter
            case tkKind peek of
                TkImplicitRef _ -> parseImplicitLet ctx cur0
                _               -> do
                    (binds, curEnd) <- bracedLetBinds curAfter []
                    finishLet binds curEnd
        _ -> do
            -- Layout mode: collect all bindings at the same column,
            -- stopping when we see `in` or a token at a smaller column.
            -- This handles both single bindings and multi-binding lets like:
            --   let a = 1
            --       b = 2
            --   in ...
            let bindCol = tkCol firstTok
            (items, curEnd) <- layoutLetItems bindCol cur0 []
            finishLetItems items curEnd
  where
    -- A LetItem is either a normal (name, expr) binding or a pattern binding
    -- (pat, rhs) that needs desugaring at the `in` site.
    -- We represent pattern bindings as Right (Pat, Expr).
    -- Normal bindings are Left (Name, Expr).

    finishLetItems items curEnd = do
        let (inTok, curIn) = nextSig ctx curEnd
        case tkKind inTok of
            TkIn -> pure ()
            _    -> parseErr ctx "expected `in` in let-binding" inTok
        (body0, curBody) <- parseExpr ctx curIn
        -- Desugar all items: normal bindings accumulate into ELet,
        -- pattern bindings get a fresh temp name + body wrapped in ECase.
        let (normalBinds, body1) = foldl desugarItem ([], body0) (reverse items)
        pure (if null normalBinds then body1 else ELet normalBinds body1, curBody)
      where
        desugarItem (normalAcc, bodyAcc) (Left (n, e)) =
            ((n, e) : normalAcc, bodyAcc)
        desugarItem (normalAcc, bodyAcc) (Right (pat, rhsE)) =
            -- let (a, b) = rhs in body
            -- desugars to: let $patN = rhs in case $patN of { pat -> body; _ -> error }
            let tmpName = BC.pack ("$pat" ++ show (length normalAcc))
                newBody = ECase (EVar tmpName)
                            [ Alt pat bodyAcc
                            , Alt PWild (EApp (EVar "error")
                                          (stringToConsList "Non-exhaustive pattern in let"))
                            ]
            in ((tmpName, rhsE) : normalAcc, newBody)

    finishLet binds curEnd = do
        let (inTok, curIn) = nextSig ctx curEnd
        case tkKind inTok of
            TkIn -> pure ()
            _    -> parseErr ctx "expected `in` in let-binding" inTok
        (body, curBody) <- parseExpr ctx curIn
        pure (ELet binds body, curBody)

    -- | Check whether the current position starts a type signature line
    -- @name[, name]* :: type@. If so, skip the sig body and return @Just curAfter@.
    -- Otherwise return @Nothing@ (caller should parse it as a value binding).
    trySkipLetSig bindCol cur = do
        let (tok1, cur1) = nextSig ctx cur
        case tkKind tok1 of
            TkIdent _ ->
                let skipNames c = do
                        let (t, c') = nextSig ctx c
                        case tkKind t of
                            TkComma ->
                                let (t2, c2) = nextSig ctx c' in
                                case tkKind t2 of
                                    TkIdent _ -> skipNames c2
                                    _         -> pure Nothing
                            TkDColon -> do
                                curAfter <- skipTypeSigBody ctx bindCol c'
                                pure (Just curAfter)
                            _ -> pure Nothing   -- not a sig
                in skipNames cur1
            _ -> pure Nothing

    -- Parse one let-binding (name + params + = or | guards + body).
    -- Handles:
    --   name = expr
    --   name params = expr
    --   name params | guard = expr | guard = expr
    --   (pat, ...) = expr            (pattern binding, returned as Right)
    parseOneLetItem cur = do
        let (nameTok, cur1) = nextSig ctx cur
        case tkKind nameTok of
            TkIdent n -> do
                (params, cur2) <- collectLetParams ctx cur1 []
                let (sepTok, cur3) = nextSig ctx cur2
                case tkKind sepTok of
                    TkEq -> do
                        (e, cur4) <- parseExpr ctx cur3
                        pure (Left (n, wrapParams params e), cur4)
                    TkBar -> do
                        (branches, cur4) <- parseLetGuardBranches ctx cur3 []
                        let e = desugarClauses [(params, RhsGuards branches)] (length params)
                        pure (Left (n, e), cur4)
                    _ -> parseErr ctx "expected `=` or `|` in let-binding" sepTok
            _ | startsPat (tkKind nameTok) -> do
                -- Pattern binding: (a, b) = expr  or  Con x = expr, etc.
                (pat, cur2) <- parseSubPat ctx cur
                let (eqTok, cur3) = nextSig ctx cur2
                case tkKind eqTok of
                    TkEq -> do
                        (e, cur4) <- parseExpr ctx cur3
                        pure (Right (pat, e), cur4)
                    _ -> parseErr ctx "expected `=` in pattern let-binding" eqTok
              | otherwise -> parseErr ctx "expected identifier or pattern after `let`" nameTok

    -- Parse `| guard = expr` branches, returning when no more `|` is seen.
    parseLetGuardBranches ctx cur acc = do
        (g, cur1) <- parseExpr ctx cur
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` after guard in let" eqTok
        (b, cur3) <- parseExpr ctx cur2
        let acc' = acc ++ [(g, b)]
        let (sep, cur4) = nextSig ctx cur3
        case tkKind sep of
            TkBar -> parseLetGuardBranches ctx cur4 acc'
            _     -> pure (acc', cur3)

    -- Collect layout-mode let items (bindings and pattern bindings).
    -- All items must be at `bindCol`.
    -- Stop when we see `in`, EOF, or a token at a column < bindCol.
    -- Type signature lines (@name :: type@) are silently skipped.
    layoutLetItems bindCol cur acc = do
        mSkip <- trySkipLetSig bindCol cur
        case mSkip of
            Just cur' -> do
                -- Type sig skipped; continue if more bindings follow.
                let (peek, _) = nextSig ctx cur'
                case tkKind peek of
                    TkIn  -> pure (reverse acc, cur')
                    TkEof -> pure (reverse acc, cur')
                    _ | tkCol peek == bindCol ->
                            layoutLetItems bindCol cur' acc
                      | otherwise -> pure (reverse acc, cur')
            Nothing -> do
                (item, cur') <- parseOneLetItem cur
                let acc' = item : acc
                -- Peek at the next significant token.
                let (peek, _) = nextSig ctx cur'
                case tkKind peek of
                    TkIn  -> pure (reverse acc', cur')
                    TkEof -> pure (reverse acc', cur')
                    _ | tkCol peek == bindCol && tkKind peek /= TkIn ->
                            -- Same column and not `in` — another binding follows.
                            layoutLetItems bindCol cur' acc'
                      | otherwise -> pure (reverse acc', cur')

    bracedLetBinds cur acc = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkIdent n -> pure n
            _         -> parseErr ctx "expected identifier in let-binding" nameTok
        (params, cur2) <- collectLetParams ctx cur1 []
        let (eqTok, cur3) = nextSig ctx cur2
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` in let-binding" eqTok
        (e, cur4) <- parseExpr ctx cur3
        let (sep, curN) = nextSig ctx cur4
        case tkKind sep of
            TkSemi   -> bracedLetBinds curN ((name, wrapParams params e) : acc)
            TkRBrace -> pure (reverse ((name, wrapParams params e) : acc), curN)
            _        -> parseErr ctx "expected `;` or `}` in let-block" sep

-- | Parse @let ?x = e; ?y = f in body@ — one or more implicit-param
-- bindings followed by @in body@. Produces 'EImplicitLet'.
--
-- Grammar (layout or explicit braces both work):
--   let { ?x = e ; ?y = f } in body
--   let ?x = e in body
parseImplicitLet :: Ctx -> Cursor -> IO (Expr, Cursor)
parseImplicitLet ctx cur0 = do
    let (firstTok, curAfter) = nextSig ctx cur0
    (iBinds, curEnd) <- case tkKind firstTok of
        TkLBrace -> bracedIPBinds curAfter []
        _        -> singleIPBind cur0
    let (inTok, curIn) = nextSig ctx curEnd
    case tkKind inTok of
        TkIn -> pure ()
        _    -> parseErr ctx "expected `in` after implicit let" inTok
    (body, curBody) <- parseExpr ctx curIn
    pure (EImplicitLet iBinds body, curBody)
  where
    singleIPBind cur = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkImplicitRef n -> pure n
            _               -> parseErr ctx "expected `?name` in implicit let" nameTok
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` in implicit let" eqTok
        (e, cur3) <- parseExpr ctx cur2
        pure ([(name, e)], cur3)

    bracedIPBinds cur acc = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkImplicitRef n -> pure n
            _               -> parseErr ctx "expected `?name` in implicit let" nameTok
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` in implicit let" eqTok
        (e, cur3) <- parseExpr ctx cur2
        let (sep, curN) = nextSig ctx cur3
        case tkKind sep of
            TkSemi   -> bracedIPBinds curN ((name, e) : acc)
            TkRBrace -> pure (reverse ((name, e) : acc), curN)
            _        -> parseErr ctx "expected `;` or `}` in implicit let" sep

parseCase :: Ctx -> Cursor -> IO (Expr, Cursor)
parseCase ctx cur0 = do
    (scrut, curS) <- parseApp ctx cur0
    let (ofTok, curO) = nextSig ctx curS
    case tkKind ofTok of
        TkOf -> pure ()
        _    -> parseErr ctx "expected `of` in case-expression" ofTok
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
        _        -> parseErr ctx "expected `;` or `}` in case alts" sep

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
        _       -> parseErr ctx "expected `->` in case alternative" arr
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
        TkConId n  -> do
            (qname, cur2) <- readQualConId ctx n tok cur1
            collectArgs ctx (stripQualifier qname) [] cur2
        _          -> parseSubPat ctx cur

collectArgs :: Ctx -> Name -> [Pat] -> Cursor -> IO (Pat, Cursor)
collectArgs ctx name acc cur =
    let (tok, cur') = nextSig ctx cur in
    case tkKind tok of
        -- Record-syntax pattern: Con { f1 = p1, f2 = p2 } or Con {..}
        TkLBrace -> do
            (fieldPats, curEnd, isWild) <- parseRecordPatFields ctx name cur' []
            if isWild
                then pure (PRecordWild name, curEnd)
                else pure (PRecord name fieldPats, curEnd)
        _ | startsPat (tkKind tok) -> do
            (sp, cur'') <- parseSubPat ctx cur
            collectArgs ctx name (sp : acc) cur''
          | otherwise -> pure (PCon name (reverse acc), cur)

-- | A sub-pattern. Handles as-patterns @name\@sub@, bang patterns,
-- tuples, negative-literal patterns, etc.
parseSubPat :: Ctx -> Cursor -> IO (Pat, Cursor)
parseSubPat ctx cur = do
    let (tok, cur1) = nextSig ctx cur
    case tkKind tok of
        TkBang -> do
            (p, curN) <- parseSubPat ctx cur1
            pure (PBang p, curN)
        -- Lazy/irrefutable pattern: ~pat.  Under ihc's already-lazy
        -- evaluator ~pat has the same runtime semantics as pat (we don't
        -- enforce strictness without an explicit bang), so strip the
        -- tilde at parse time and return the inner pattern unchanged.
        TkSymOp op | op == BC.pack "~" -> parseSubPat ctx cur1
        TkIdent n -> do
            -- Potential as-pattern: ident '@' sub
            -- But '@' followed by TkSymOp is an infix operator (@?=, @=?, etc.),
            -- not an as-pattern.  Check the token AFTER '@' before committing.
            let (peek, curP) = nextSig ctx cur1
            case tkKind peek of
                TkAt ->
                    let (afterAt, _) = nextSig ctx curP
                    in case tkKind afterAt of
                        TkSymOp _ -> pure (PVar n, cur1)  -- @op: infix LHS, not as-pat
                        _         -> do
                            (sub, curN) <- parseSubPat ctx curP
                            pure (PAs n sub, curN)
                _    -> pure (PVar n, cur1)
        TkPrimId n   -> pure (PVar n, cur1)   -- e.g. state# param in primop binding
        TkUnderscore -> pure (PWild, cur1)
        TkInt n      -> pure (PLit (LInt (fromInteger n)), cur1)
        TkFloat d    -> pure (PLit (LFloat d), cur1)
        TkStr s      -> pure (PLit (LStr s), cur1)
        TkChar c     -> pure (PLit (LChar c), cur1)
        TkConId n    -> do
            -- Handle qualified constructor: M.Ctor or M.N.Ctor
            -- Read adjacent dots/segments; strip the qualifier for pattern matching.
            (qname, cur2) <- readQualConId ctx n tok cur1
            pure (PCon (stripQualifier qname) [], cur2)
        TkLParen     -> parseParenPat ctx cur1
        TkLBracket   -> parseListPat ctx cur1
        TkLUnbox     -> parseUnboxedTuplePat ctx cur1
        TkMinus -> do
            let (n, cur2) = nextSig ctx cur1
            case tkKind n of
                TkInt i   -> pure (PLit (LInt (fromInteger (negate i))), cur2)
                TkFloat d -> pure (PLit (LFloat (negate d)), cur2)
                _         -> parseErr ctx "expected number after `-` in pattern" n
        _ -> parseErr ctx "expected pattern (Int, String, _, ident, or constructor)" tok

-- | Parenthesised pattern: could be a single pattern @(p)@, a tuple
-- @(p,q,r)@, unit @()@, or a view pattern @(expr -> p)@.
-- The opening @(@ has been consumed.
parseParenPat :: Ctx -> Cursor -> IO (Pat, Cursor)
parseParenPat ctx cur0 = do
    let (peek, curP) = nextSig ctx cur0
    case tkKind peek of
        TkRParen -> pure (PCon "()" [], curP)
        -- ViewPatterns: peek ahead for `->` at paren-depth 0.
        -- If found, parse the LHS as an expression and RHS as a pattern.
        _ | peekViewArrow ctx cur0 -> do
            (viewFn, cur1) <- parseExpr ctx cur0
            let (arrTok, cur2) = nextSig ctx cur1
            case tkKind arrTok of
                TkArrow -> pure ()
                _       -> parseErr ctx "expected `->` in view pattern" arrTok
            (viewPat, cur3) <- parseTopPat ctx cur2
            let (closeTok, cur4) = nextSig ctx cur3
            case tkKind closeTok of
                TkRParen -> pure (PView viewFn viewPat, cur4)
                _        -> parseErr ctx "expected `)` after view pattern" closeTok
          | otherwise -> do
            (first, cur1) <- parseTopPat ctx cur0
            let (sep, cur2) = nextSig ctx cur1
            case tkKind sep of
                TkRParen -> pure (first, cur2)
                TkComma  -> gatherTuple ctx [first] cur2
                -- (pat :: Type) — type annotation in pattern; discard the type.
                TkDColon -> do
                    cur3 <- skipToCloseParen ctx cur2
                    pure (first, cur3)
                _        -> parseErr ctx "expected `)` or `,` in pattern" sep

-- | Read a (possibly qualified) constructor name starting from the first
-- segment @n@ (already consumed, token @tok@). Returns the full qualified
-- name and the cursor positioned after the last segment consumed.
-- E.g. reading \"M.Just\" returns (\"M.Just\", curAfter) where curAfter is
-- positioned after \"Just\". The caller may then call 'stripQualifier' to
-- obtain the unqualified name for pattern matching.
readQualConId :: Ctx -> Name -> Token -> Cursor -> IO (Name, Cursor)
readQualConId ctx = go
  where
    src = ctxSrc ctx
    go acc lastTok cur =
        let (dotTok, curAfterDot) = nextToken src cur in
        case tkKind dotTok of
            TkDot | tkStart dotTok == tkEnd lastTok ->
                let (segTok, curAfterSeg) = nextToken src curAfterDot in
                case tkKind segTok of
                    TkConId n | tkStart segTok == tkEnd dotTok ->
                        go (acc <> BC.pack "." <> n) segTok curAfterSeg
                    _ -> pure (acc, cur)   -- dot not followed by ConId; stop
            _ -> pure (acc, cur)

-- | Skip tokens up to and including the next unmatched @)@ at depth 0.
-- Used to skip type annotations in patterns after @::@.
skipToCloseParen :: Ctx -> Cursor -> IO Cursor
skipToCloseParen ctx = go (0 :: Int)
  where
    go !depth cur =
        let (tok, cur') = nextSig ctx cur in
        case tkKind tok of
            TkEof    -> pure cur   -- shouldn't happen; stop anyway
            TkLParen -> go (depth + 1) cur'
            TkLBracket -> go (depth + 1) cur'
            TkRBracket -> go (depth - 1) cur'
            TkRParen
                | depth == 0 -> pure cur'   -- consumed the closing ')'
                | otherwise  -> go (depth - 1) cur'
            _ -> go depth cur'

-- | Check whether there is a @->@ token at paren-depth 0 before the
-- matching @)@. Used to distinguish view patterns from regular paren patterns.
peekViewArrow :: Ctx -> Cursor -> Bool
peekViewArrow ctx cur0 = go cur0 (0 :: Int)
  where
    go cur !depth =
        let (tok, cur') = nextSig ctx cur in
        case tkKind tok of
            TkEof    -> False
            TkRParen | depth == 0 -> False
            TkRParen              -> go cur' (depth - 1)
            TkLParen              -> go cur' (depth + 1)
            TkLBracket            -> go cur' (depth + 1)
            TkRBracket            -> go cur' (depth - 1)
            TkLBrace              -> go cur' (depth + 1)
            TkRBrace              -> go cur' (depth - 1)
            TkComma  | depth == 0 -> False   -- tuple pattern, not view
            TkArrow  | depth == 0 -> True
            _                     -> go cur' depth

gatherTuple :: Ctx -> [Pat] -> Cursor -> IO (Pat, Cursor)
gatherTuple ctx acc cur = do
    (p, cur1) <- parseTopPat ctx cur
    let (sep, cur2) = nextSig ctx cur1
    case tkKind sep of
        TkComma  -> gatherTuple ctx (p : acc) cur2
        TkRParen -> pure (PTuple (reverse (p : acc)), cur2)
        _        -> parseErr ctx "expected `,` or `)` in tuple pattern" sep

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
        _ -> parseErr ctx "expected `,` or `]` in list pattern" tok

-- | Parse an unboxed tuple pattern @(# p1, p2, ... #)@.
-- The opening @(#@ token has already been consumed.
-- Desugars to @PCon "(#,#)" [p1, p2]@ etc., matching the runtime
-- representation used by the MutVar# primops (VCon "(#,#)" [...]).
parseUnboxedTuplePat :: Ctx -> Cursor -> IO (Pat, Cursor)
parseUnboxedTuplePat ctx cur0 =
    if isUnboxClose ctx cur0
        then pure (PCon "(##)" [], skipUnboxClose ctx cur0)
        else do
            (first, cur1) <- parseTopPat ctx cur0
            gatherUnboxedTuplePat ctx [first] cur1

gatherUnboxedTuplePat :: Ctx -> [Pat] -> Cursor -> IO (Pat, Cursor)
gatherUnboxedTuplePat ctx acc cur =
    if isUnboxClose ctx cur
        then
            let ps    = reverse acc
                arity = length ps
                conNm = BC.pack $ "(#" <> replicate (arity - 1) ',' <> "#)"
            in pure (PCon conNm ps, skipUnboxClose ctx cur)
        else do
            let (tok, cur1) = nextSig ctx cur
            case tkKind tok of
                TkComma -> do
                    (p, cur2) <- parseTopPat ctx cur1
                    gatherUnboxedTuplePat ctx (p : acc) cur2
                _ -> parseErr ctx "expected `,` or `#)` in unboxed tuple pattern" tok

-- | Parse record-pattern field list @{ f1 = p1, f2, .. }@. The opening
-- @{@ has already been consumed. Returns @(fields, cursorAfterClose, isWild)@.
-- Supports NamedFieldPuns: @{ x }@ → @[("x", PVar "x")]@.
-- Supports RecordWildCards: @{..}@ sets @isWild = True@.
parseRecordPatFields :: Ctx -> Name -> Cursor -> [(Name, Pat)] -> IO ([(Name, Pat)], Cursor, Bool)
parseRecordPatFields ctx _conName cur acc = do
    let (tok, cur') = nextSig ctx cur
    case tkKind tok of
        TkRBrace -> pure (reverse acc, cur', False)
        TkComma  -> parseRecordPatFields ctx _conName cur' acc
        TkDotDot -> do
            let (closeTok, curClose) = nextSig ctx cur'
            case tkKind closeTok of
                TkRBrace -> pure (reverse acc, curClose, True)
                _        -> parseErr ctx "expected `}` after `..` in record pattern" closeTok
        TkIdent fname -> do
            let (eqTok, cur2) = nextSig ctx cur'
            case tkKind eqTok of
                TkEq -> do
                    (p, cur3) <- parseTopPat ctx cur2
                    parseRecordPatFields ctx _conName cur3 ((fname, p) : acc)
                -- NamedFieldPuns: { x } → { x = x }
                -- Note: use cur' (not cur2) so we don't consume the non-`=` token.
                _ -> parseRecordPatFields ctx _conName cur' ((fname, PVar fname) : acc)
        TkEof -> pure (reverse acc, cur', False)
        _ -> parseErr ctx "expected field name, `..`, or `}` in record pattern" tok

startsPat :: TokenKind -> Bool
startsPat (TkInt _)    = True
startsPat (TkFloat _)  = True
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
startsPat TkLUnbox     = True   -- (# a, b #) unboxed tuple patterns
startsPat (TkSymOp op) = op == BC.pack "~"   -- lazy/irrefutable ~pat
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
    applyOp "$!" a b   = EApp (EApp (EVar "seq") b) (EApp a b)
        -- $! is strict application: f $! x = x `seq` f x
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
            case readBacktickName ctx cur' of
                Just (n, curId) ->
                    let (closeTok, curClose) = nextSig ctx curId in
                    case tkKind closeTok of
                        TkBacktick ->
                            let key    = BC.pack "`" <> n <> BC.pack "`"
                                (a, p) = lookupFixity (ctxFixity ctx) key
                            in Just (n, a, p, curClose)
                        _ -> Nothing
                Nothing -> Nothing
        -- '@'-prefixed operator: @?=, @=?, etc.
        -- '@' (0x40) is not in isOpChar so it produces TkAt; the suffix is
        -- a separate TkSymOp.  Combine them into a single operator name.
        TkAt ->
            let (nextTok, cur'') = nextSig ctx cur'
            in case tkKind nextTok of
                TkSymOp suf ->
                    let name   = BC.pack "@" <> suf
                        (a, p) = lookupFixity (ctxFixity ctx) name
                    in Just (name, a, p, cur'')
                _ -> Nothing
        _ -> case tokenOpName (tkKind tok) of
            Nothing   -> Nothing
            Just name ->
                let (a, p) = lookupFixity (ctxFixity ctx) name
                in Just (name, a, p, cur')

-- | Parse an identifier-like name inside backticks, allowing qualified
-- segments such as @B.snoc@ or @Data.List.elem@.
readBacktickName :: Ctx -> Cursor -> Maybe (Name, Cursor)
readBacktickName ctx = goFirst
  where
    goFirst cur =
        let (tok, cur1) = nextSig ctx cur
        in case tkKind tok of
            TkIdent n -> goRest n cur1
            TkConId n -> goRest n cur1
            _         -> Nothing

    goRest acc cur =
        let (dotTok, curDot) = nextSig ctx cur
        in case tkKind dotTok of
            TkDot ->
                let (segTok, curSeg) = nextSig ctx curDot
                in case tkKind segTok of
                    TkIdent n | tkStart segTok == tkEnd dotTok ->
                        goRest (acc <> BC.pack "." <> n) curSeg
                    TkConId n | tkStart segTok == tkEnd dotTok ->
                        goRest (acc <> BC.pack "." <> n) curSeg
                    _ -> Just (acc, cur)
            _ -> Just (acc, cur)

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
        let (tok, cur') = nextSig ctx cur in
        case tkKind tok of
            -- TypeApplications: @T is a type-level hint; discard and continue.
            -- But @?=, @=?, etc. are operator-infix uses, not type applications:
            -- peek at the token right after '@' (without skipping whitespace).
            -- If it's a TkSymOp, leave for the Pratt operator parser.
            TkAt ->
                let (nextTok, _) = nextToken (ctxSrc ctx) cur'
                in case tkKind nextTok of
                    TkSymOp _ -> pure (fn, cur)   -- operator, hand off to Pratt
                    _ -> do
                        cur'' <- skipTypeArg ctx cur'
                        loop fn cur''
            -- Record-update: expr { field = val, ... }
            -- Disambiguate from block syntax: only treat as record-update when
            -- we see '{' immediately followed by 'ident =' or '}'.
            TkLBrace | isRecordUpdateBrace ctx cur' -> do
                    (fields, curEnd) <- parseRecordUpdateFields ctx cur' []
                    loop (ERecordUpdate fn fields) curEnd
            _ | startsAtom (tkKind tok) && tkCol tok > ctxMinCol ctx -> do
                    (arg, curA) <- parseAtom ctx cur
                    loop (EApp fn arg) curA
              | otherwise -> pure (fn, cur)

-- | Peek ahead after @{@ to decide whether this is a record-update
-- expression @expr { f = e, ... }@. Returns 'True' when the tokens
-- immediately after @{@ are @ident =@, @ident }@, @ident ,@ (field-update
-- or NamedFieldPun syntax), or @}@ (empty update).
-- Returns 'False' otherwise (e.g. a do-block @{ stmt; }@).
isRecordUpdateBrace :: Ctx -> Cursor -> Bool
isRecordUpdateBrace ctx cur =
    let (t1, cur1) = nextSig ctx cur in
    case tkKind t1 of
        TkRBrace    -> True          -- empty update { }
        TkIdent _   ->
            let (t2, _) = nextSig ctx cur1 in
            case tkKind t2 of
                TkEq     -> True   -- { field = ...
                TkRBrace -> True   -- { field }  (NamedFieldPun)
                TkComma  -> True   -- { field, ... } (NamedFieldPun list)
                _        -> False
        _           -> False

-- | Parse a record-update field list @{ f1 = e1, f2 = e2, ... }@.
-- The opening @{@ has NOT been consumed; @cur@ is positioned just after @{@.
-- Supports NamedFieldPuns: @{ x }@ → @{ x = x }@.
parseRecordUpdateFields :: Ctx -> Cursor -> [(Name, Expr)] -> IO ([(Name, Expr)], Cursor)
parseRecordUpdateFields ctx cur acc = do
    let (tok, cur') = nextSig ctx cur
    case tkKind tok of
        TkRBrace -> pure (reverse acc, cur')
        TkComma  -> parseRecordUpdateFields ctx cur' acc
        TkIdent fname -> do
            let (eqTok, cur2) = nextSig ctx cur'
            case tkKind eqTok of
                TkEq -> do
                    (e, cur3) <- parseExpr ctx cur2
                    parseRecordUpdateFields ctx cur3 ((fname, e) : acc)
                -- NamedFieldPun: { x } → { x = x }
                -- Don't consume the non-`=` token (use cur', not cur2).
                _ -> parseRecordUpdateFields ctx cur' ((fname, EVar fname) : acc)
        TkEof -> pure (reverse acc, cur')
        _ -> parseErr ctx "expected field name or `}` in record update" tok

-- | Skip one type-argument after @\@@ in a type application.
-- Handles: plain ident/conid (@\@Int@, @\@a@), promoted (@\@\'Foo@),
-- and balanced paren/bracket groups (@\@(Maybe Int)@, @\@[Int]@).
skipTypeArg :: Ctx -> Cursor -> IO Cursor
skipTypeArg ctx cur0 =
    let (tok, cur1) = nextSig ctx cur0 in
    case tkKind tok of
        TkAt       -> pure cur0   -- bare '@' shouldn't nest; stop
        TkLParen   -> skipBalanced ctx cur1 TkRParen
        TkLBracket -> skipBalanced ctx cur1 TkRBracket
        -- Promoted tick: lexer reads 'X as TkChar 'X' and leaves the rest
        -- of the constructor name as a separate ident token. Skip both.
        TkChar _   ->
            let (tok2, cur2) = nextSig ctx cur1 in
            case tkKind tok2 of
                TkIdent{} -> pure cur2
                TkConId{} -> pure cur2
                _         -> pure cur1
        _ -> pure cur1  -- single token consumed (ident, conid, primid, etc.)

-- | Skip tokens until the matching close bracket/paren (depth-aware).
-- @cur0@ is positioned just after the opening bracket.
skipBalanced :: Ctx -> Cursor -> TokenKind -> IO Cursor
skipBalanced ctx cur0 close = go cur0 (1 :: Int)
  where
    go cur !d =
        let (tok, cur') = nextSig ctx cur in
        case tkKind tok of
            TkEof -> pure cur
            k | k == close -> if d == 1 then pure cur' else go cur' (d - 1)
            TkLParen    -> go cur' (d + 1)
            TkRParen    -> go cur' (d - 1)
            TkLBracket  -> go cur' (d + 1)
            TkRBracket  -> go cur' (d - 1)
            _           -> go cur' d

startsAtom :: TokenKind -> Bool
startsAtom TkInt{}         = True
startsAtom TkFloat{}       = True
startsAtom TkStr{}         = True
startsAtom TkChar{}        = True
startsAtom TkLabel{}       = True  -- Phase 3.5: #name is a valid argument
startsAtom TkLParen        = True
startsAtom TkLBracket      = True
startsAtom TkIdent{}       = True
startsAtom TkConId{}       = True
startsAtom TkPrimId{}      = True
startsAtom TkLUnbox        = True
startsAtom TkImplicitRef{} = True  -- Phase 3.6: ?name can start an atom
startsAtom TkSpliceLParen  = True  -- Phase 2.11: $( starts a TH splice
startsAtom TkOQuote        = True  -- Phase 2.12: [| starts a TH expression bracket
startsAtom TkOQuoteD       = True  -- Phase 2.12: [d| (silently skipped)
startsAtom TkOQuoteT       = True  -- Phase 2.12: [t| (silently skipped)
startsAtom TkOQuoteP       = True  -- Phase 2.12: [p| (silently skipped)
startsAtom TkOQuoteTy      = True  -- Phase 2.12: [|| (silently skipped)
startsAtom _               = False

--------------------------------------------------------------------------------
-- Atoms
--------------------------------------------------------------------------------

parseAtom :: Ctx -> Cursor -> IO (Expr, Cursor)
parseAtom ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkInt n    -> pure (ELit (LInt (fromInteger n)), cur1)
        TkFloat d  -> pure (ELit (LFloat d), cur1)
        TkStr s    -> pure (stringToConsList (BC.unpack s), cur1)
        TkChar c   -> pure (ELit (LChar c), cur1)
        TkLabel n  -> pure (ELabel n, cur1)   -- Phase 3.5: OverloadedLabels
        -- Phase 3.6: ?name in expression position -> implicit parameter reference
        -- Also support postfix dot-chain: ?ctx.field -> field ?ctx
        TkImplicitRef n -> applyRecordDots ctx tok (EImplicitRef n) cur1
        TkIdent n
            | n == "_" -> parseErr ctx "wildcard `_` in expression position" tok
            | otherwise -> applyRecordDots ctx tok (EVar n) cur1
        TkPrimId n -> pure (EVar n, cur1)
        TkConId n -> do
            -- Check for record construction: Con { f1 = v1, f2 = v2 }
            -- We use nextSig to skip whitespace so "Con { }" and "Con{}" both work.
            (qexpr, qcur) <- tryQualified ctx n tok cur1
            case qexpr of
                EVar qname ->
                    let (nextTk, curAfterBrace) = nextSig ctx qcur in
                    case tkKind nextTk of
                        TkLBrace -> do
                            -- Record literal: parse field-list then closing '}'
                            (fields, curEnd, isWild) <- parseRecordFields ctx qname curAfterBrace []
                            if isWild
                                then pure (ERecordWild qname, curEnd)
                                else pure (ERecordCon qname fields, curEnd)
                        _ -> pure (qexpr, qcur)
                _ -> pure (qexpr, qcur)
        TkLParen   -> parseParenExpr ctx tok cur1
        TkLUnbox   -> parseUnboxedTuple ctx cur1
        TkLBracket -> parseListLit ctx cur1
        -- Phase 2.11: TH splice $( expr )
        TkSpliceLParen -> do
            (inner, cur2) <- parseExpr ctx cur1
            let (closeTok, cur3) = nextSig ctx cur2
            case tkKind closeTok of
                TkRParen -> pure (ESplice inner, cur3)
                _        -> parseErr ctx "expected `)` to close splice $(...)" closeTok
        -- Phase 2.12: TemplateHaskellQuotes
        -- [| expr |]  or  [e| expr |]  — expression bracket: parse body, emit EQuote.
        TkOQuote -> do
            (inner, cur2) <- parseExpr ctx cur1
            let (closeTok, cur3) = nextSig ctx cur2
            case tkKind closeTok of
                TkCQuote -> pure (EQuote inner, cur3)
                _        -> parseErr ctx "expected `|]` to close expression bracket [|...|]" closeTok
        -- [d| ... |], [t| ... |], [p| ... |], [|| ... ||] — not yet implemented.
        -- Silently skip the body (up to the matching close) and return a placeholder.
        TkOQuoteD  -> skipQuoteBody ctx cur1 TkCQuote
        TkOQuoteT  -> skipQuoteBody ctx cur1 TkCQuote
        TkOQuoteP  -> skipQuoteBody ctx cur1 TkCQuote
        TkOQuoteTy -> skipQuoteBody ctx cur1 TkCQuoteTy
        TkEof -> parseErr ctx "unexpected end of input" tok
        _ -> parseErr ctx "unexpected token" tok

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
        -- OverloadedRecordDot: (.field) section — \$s -> field $s
        -- Must check adjacency: the '.' and 'field' have no whitespace between
        -- them. 'peek' already points to the '.' (via nextSig); we use nextToken
        -- (no trivia skip) to check whether the following ident abuts the dot.
        TkDot ->
            let src = ctxSrc ctx
                (fieldTok, curAfterField) = nextToken src curP
            in case tkKind fieldTok of
                TkIdent fname | tkStart fieldTok == tkEnd peek -> do
                    -- Adjacent: (.fname) is a record-dot section \$s -> fname $s
                    let (closeTok, curClose) = nextSig ctx curAfterField
                    case tkKind closeTok of
                        TkRParen ->
                            let n = "$s"
                            in pure (ELam n (EApp (EVar fname) (EVar n)), curClose)
                        _ -> do
                            -- (.fname expr) is unusual but parse generically.
                            (e, cur1) <- parseExpr ctx cur0
                            let (sep, cur2) = nextSig ctx cur1
                            case tkKind sep of
                                TkRParen -> pure (e, cur2)
                                _ -> parseErr ctx "expected `)` after record-dot section" sep
                _ -> do
                    -- Dot is not adjacent to an ident: treat as composition
                    -- operator `(.)` or a right section `(. f)`.
                    let (afterOp, curAfterOp) = nextSig ctx curP
                    case tkKind afterOp of
                        TkRParen -> pure (EVar ".", curAfterOp)
                        _ -> do
                            (rhs, curR) <- parseExpr ctx curP
                            let (closeTok, curC) = nextSig ctx curR
                            case tkKind closeTok of
                                TkRParen ->
                                    let n = "$s"
                                        body = EApp (EApp (EVar ".") (EVar n)) rhs
                                    in pure (ELam n body, curC)
                                _ -> parseErr ctx "expected `)` in composition section" closeTok
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
                        _ -> parseErr ctx "expected `)` in section" closeTok
          | tkKind peek == TkBacktick -> do
            -- Backtick section: (`f` e) = \$x -> f $x e
            case readBacktickName ctx curP of
                Just (fn, curId) -> do
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
                                        _ -> parseErr ctx "expected `)` in backtick section" closeTok
                        _ -> parseErr ctx "expected closing backtick in section" bt2
                Nothing ->
                    let (idTok, _) = nextSig ctx curP
                    in parseErr ctx "expected identifier in backtick section" idTok
        -- TupleSections: leading hole — @(, e, f)@ has no first expression.
        -- Collect all elements (some may be holes), then desugar with lambdas.
        TkComma -> do
            (rest, curEnd) <- gatherTupleSectionElems ctx curP []
            pure (desugarTupleSection (Nothing : rest), curEnd)
        _ -> do
            -- Fully general expression, possibly followed by:
            --   - `)`  → single expression, or unit.
            --   - `,`  → tuple or tuple section, collect more.
            --   - `::` → type annotation, swallow to matching `)`.
            --   - an operator + `)` → left section.
            (e, cur1) <- parseExpr ctx cur0
            let (sep, cur2) = nextSig ctx cur1
            case tkKind sep of
                TkRParen -> pure (e, cur2)
                TkComma  -> do
                    -- Check for tuple section: next token is `,` or `)` → hole.
                    (rest, curEnd) <- gatherTupleSectionElems ctx cur2 []
                    let elems = Just e : rest
                    if any isTsHole elems
                        then pure (desugarTupleSection elems, curEnd)
                        else pure (ETuple (map fromJustTs elems), curEnd)
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
                        _ -> parseErr ctx "expected `)` after operator in section" afterOp
                  | tkKind sep == TkBacktick -> do
                    -- Left backtick section: (e `f`)
                    case readBacktickName ctx cur2 of
                        Just (fn, curId) -> do
                            let (bt2, curB2) = nextSig ctx curId
                            case tkKind bt2 of
                                TkBacktick -> do
                                    let (afterOp, curAfterOp) = nextSig ctx curB2
                                    case tkKind afterOp of
                                        TkRParen ->
                                            let n = "$s"
                                                body = EApp (EApp (EVar fn) e) (EVar n)
                                            in pure (ELam n body, curAfterOp)
                                        _ -> parseErr ctx "expected `)` after left backtick section" afterOp
                                _ -> parseErr ctx "expected closing backtick in left section" bt2
                        Nothing ->
                            let (idTok, _) = nextSig ctx cur2
                            in parseErr ctx "expected identifier in backtick section" idTok
                _ -> parseErr ctx "expected `)` or `,` in parenthesised expression" sep

-- | Collect elements of a tuple section. Each element is either a
-- 'Just expr' (present) or 'Nothing' (hole — a lambda-bound argument).
-- The cursor is positioned just after a @,@ when called.
-- Collects elements and terminating @)@.
gatherTupleSectionElems :: Ctx -> Cursor -> [Maybe Expr] -> IO ([Maybe Expr], Cursor)
gatherTupleSectionElems ctx cur acc = do
    let (peek, curP) = nextSig ctx cur
    case tkKind peek of
        TkRParen ->
            -- Trailing comma before `)`: last element is a hole.
            pure (reverse (Nothing : acc), curP)
        TkComma ->
            -- Two commas in a row: current slot is a hole.
            gatherTupleSectionElems ctx curP (Nothing : acc)
        _ -> do
            (e, cur1) <- parseExpr ctx cur
            let (sep, cur2) = nextSig ctx cur1
            case tkKind sep of
                TkComma  -> gatherTupleSectionElems ctx cur2 (Just e : acc)
                TkRParen -> pure (reverse (Just e : acc), cur2)
                _        -> parseErr ctx "expected `,` or `)` in tuple section" sep

isTsHole :: Maybe a -> Bool
isTsHole Nothing  = True
isTsHole (Just _) = False

fromJustTs :: Maybe Expr -> Expr
fromJustTs (Just e) = e
fromJustTs Nothing  = error "IHC.Parser: impossible hole in non-section tuple"

-- | Desugar a tuple section into a lambda. Each 'Nothing' slot becomes
-- a fresh lambda parameter @$tsN@. The body is an 'ETuple'.
desugarTupleSection :: [Maybe Expr] -> Expr
desugarTupleSection elems =
    let holes = [i | (i, Nothing) <- zip [0 :: Int ..] elems]
        names = [BC.pack ("$ts" ++ show i) | i <- holes]
        nameMap = Map.fromList (zip holes names)
        body  = ETuple [ case me of
                            Just e  -> e
                            Nothing -> EVar (nameMap Map.! i)
                       | (i, me) <- zip [0 :: Int ..] elems
                       ]
    in foldr ELam body names


-- | Parse a record-field list @{ f1 = e1, f2 = e2, ... }@. The opening
-- @{@ has already been consumed. Returns the fields and cursor after @}@.
-- Supports NamedFieldPuns (@{ x }@ → @{ x = x }@) and RecordWildCards
-- (@{..}@ → an 'ERecordWild' placeholder resolved by the scheduler).
parseRecordFields :: Ctx -> Name -> Cursor -> [(Name, Expr)] -> IO ([(Name, Expr)], Cursor, Bool)
parseRecordFields ctx conName cur acc = do
    let (tok, cur') = nextSig ctx cur
    case tkKind tok of
        TkRBrace -> pure (reverse acc, cur', False)
        TkComma  -> parseRecordFields ctx conName cur' acc
        -- RecordWildCards: {..} — return a sentinel flag; the caller wraps
        -- the whole ERecordCon into an ERecordWild.
        TkDotDot -> do
            let (closeTok, curClose) = nextSig ctx cur'
            case tkKind closeTok of
                TkRBrace -> pure (reverse acc, curClose, True)
                _        -> parseErr ctx "expected `}` after `..` in record construction" closeTok
        TkIdent fname -> do
            let (eqTok, cur2) = nextSig ctx cur'
            case tkKind eqTok of
                TkEq -> do
                    (e, cur3) <- parseExpr ctx cur2
                    parseRecordFields ctx conName cur3 ((fname, e) : acc)
                -- NamedFieldPuns: { x } or { x, y } — no `=`, bind to same name.
                -- Note: use cur' (not cur2) so we don't consume the non-`=` token.
                _ -> parseRecordFields ctx conName cur' ((fname, EVar fname) : acc)
        TkEof -> pure (reverse acc, cur', False)
        _ -> parseErr ctx "expected field name or `}` in record literal" tok

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
                        _ -> parseErr ctx "expected `,` or `#)` in unboxed tuple" sep
  where
    gatherUnboxed c cur acc = do
        (e, cur1) <- parseExpr c cur
        if isUnboxClose c cur1
            then pure (reverse (e : acc), skipUnboxClose c cur1)
            else do
                let (sep, cur2) = nextSig c cur1
                case tkKind sep of
                    TkComma -> gatherUnboxed c cur2 (e : acc)
                    _       -> parseErr ctx "expected `,` or `#)` in unboxed tuple" sep

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

-- | OverloadedRecordDot: after parsing a variable atom, check if it is
-- immediately followed by @.field@ (no whitespace). If so, desugar:
--
-- > x.f        ->  f x
-- > x.f.g      ->  g (f x)
--
-- Adjacency is required: @tkStart dotTok == tkEnd lastTok@ and
-- @tkStart fieldTok == tkEnd dotTok@. This mirrors the same check used
-- in 'tryQualified' for qualified names (e.g. @Foo.bar@).
applyRecordDots :: Ctx -> Token -> Expr -> Cursor -> IO (Expr, Cursor)
applyRecordDots ctx lastTok expr cur =
    let src = ctxSrc ctx
        (dotTok, curAfterDot) = nextToken src cur
    in case tkKind dotTok of
        TkDot | tkStart dotTok == tkEnd lastTok ->
            let (fieldTok, curAfterField) = nextToken src curAfterDot in
            case tkKind fieldTok of
                TkIdent fname | tkStart fieldTok == tkEnd dotTok ->
                    -- Desugar: expr.fname -> fname expr, then recurse for chaining.
                    applyRecordDots ctx fieldTok (EApp (EVar fname) expr) curAfterField
                _ -> pure (expr, cur)
        _ -> pure (expr, cur)

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
            gather first cur2
  where
    gather first cur = do
        let (tok, cur1) = nextSig ctx cur
        case tkKind tok of
            -- Range: [first .. hi]  or  [first .. ]  (infinite — not yet supported)
            TkDotDot -> do
                let (peek, _) = nextSig ctx cur1
                case tkKind peek of
                    TkRBracket ->
                        -- [first ..] — infinite range, not yet supported; skip
                        pure (buildCons [first], snd (nextSig ctx cur1))
                    _ -> do
                        (hi, cur3) <- parseExpr ctx cur1
                        let (close, cur4) = nextSig ctx cur3
                        case tkKind close of
                            TkRBracket ->
                                pure (EApp (EApp (EVar "enumFromTo") first) hi, cur4)
                            _ -> parseErr ctx "expected `]` after range upper bound" close
            -- Step range: after gathering [first, second, we see '..'
            TkComma -> do
                (second, cur2) <- parseExpr ctx cur1
                let (peek2, cur3) = nextSig ctx cur2
                case tkKind peek2 of
                    TkDotDot -> do
                        let (peek3, _) = nextSig ctx cur3
                        case tkKind peek3 of
                            TkRBracket ->
                                -- [first, second ..] — infinite stepped range, skip
                                pure (buildCons [first, second], snd (nextSig ctx cur3))
                            _ -> do
                                (hi, cur4) <- parseExpr ctx cur3
                                let (close, cur5) = nextSig ctx cur4
                                case tkKind close of
                                    TkRBracket ->
                                        pure ( EApp (EApp (EApp (EVar "enumFromThenTo")
                                                               first) second) hi
                                             , cur5 )
                                    _ -> parseErr ctx "expected `]` after range upper bound" close
                    -- Plain list: continue gathering
                    _ -> gatherMore [second, first] cur2
            TkRBracket -> pure (buildCons [first], cur1)
            -- List comprehension @[ e | q1, q2, ... ]@ — swallow to ']'.
            TkBar -> do
                curEnd <- skipToCloseBracket ctx cur1
                pure (buildCons [first], curEnd)
            _ -> parseErr ctx "expected `,`, `..`, or `]` in list literal" tok

    gatherMore acc cur = do
        let (tok, cur1) = nextSig ctx cur
        case tkKind tok of
            TkComma -> do
                (e, cur2) <- parseExpr ctx cur1
                gatherMore (e : acc) cur2
            TkRBracket -> pure (buildCons (reverse acc), cur1)
            TkBar -> do
                curEnd <- skipToCloseBracket ctx cur1
                pure (buildCons (reverse acc), curEnd)
            _ -> parseErr ctx "expected `,` or `]` in list literal" tok

    buildCons :: [Expr] -> Expr
    buildCons []     = EVar "[]"
    buildCons (e:es) = EApp (EApp (EVar ":") e) (buildCons es)

-- | Skip the body of an unsupported TH quote bracket up to (and including)
-- the matching close token (@TkCQuote@ = @|]@ or @TkCQuoteTy@ = @||]@).
-- Returns a placeholder expression so the surrounding code can continue
-- parsing without a hard error. Tracks bracket depth for nested quotes.
skipQuoteBody :: Ctx -> Cursor -> TokenKind -> IO (Expr, Cursor)
skipQuoteBody ctx cur0 closeTk = go cur0 (0 :: Int)
  where
    placeholder = EQuote (ELit (LStr "<unsupported-quote>"))

    go cur !depth =
        let (tok, cur') = nextSig ctx cur in
        case tkKind tok of
            TkEof -> pure (placeholder, cur)
            k | k == closeTk && depth == 0 -> pure (placeholder, cur')
              | k == closeTk               -> go cur' (depth - 1)
            TkOQuote   -> go cur' (depth + 1)
            TkOQuoteD  -> go cur' (depth + 1)
            TkOQuoteT  -> go cur' (depth + 1)
            TkOQuoteP  -> go cur' (depth + 1)
            TkOQuoteTy -> go cur' (depth + 1)
            TkLParen   -> go cur' (depth + 1)
            TkRParen   -> go cur' (max 0 (depth - 1))
            TkLBracket -> go cur' (depth + 1)
            TkRBracket -> go cur' (max 0 (depth - 1))
            TkLBrace   -> go cur' (depth + 1)
            TkRBrace   -> go cur' (max 0 (depth - 1))
            _          -> go cur' depth

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

parseErr :: Ctx -> String -> Token -> IO a
parseErr ctx msg tok =
    throwIO (ParseError
        { peFile = srcName (ctxSrc ctx)
        , peLine = tkLine tok
        , peCol  = tkCol  tok
        , peMsg  = msg <> "; saw " <> show (tkKind tok)
        })
