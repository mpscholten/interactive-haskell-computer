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
    , parseExprAtEof
    , parsePatIn
    , ParseError(..)
    , FixityTable
    , Assoc(..)
    , defaultFixityTable
    , scanFixityDecls
    ) where

import Control.Exception (Exception(..), catch, throwIO, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isSpace)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import IHC.AST
import IHC.Lexer
import IHC.Scan (Clause(..))
import IHC.Source
import IHC.StringUtils (isAsciiSpace)

-- | Run a parser action, converting any pure 'LexError' it forces into a
-- 'ParseError' so callers see one error type. The lexer raises 'LexError'
-- via 'throw' (pure) for malformed literals (out-of-range char escape,
-- empty exponent); without this bridge the user would get a bare
-- @LexError ...@ instead of a @ParseError@.
liftLex :: IO a -> IO a
liftLex action = action `catch` \(LexError file line col msg) ->
    throwIO (ParseError file line col msg)

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
    , ("!",   (AssocL, 9))
    , (".",   (AssocR, 9))
    , ("!!",  (AssocL, 9))
    -- GHC.Prim unboxed operators.  GHC.Prim has no .hs source, so its
    -- primop fixities are never scanned from a module — they must be
    -- seeded here, or mixed expressions mis-parse at the default
    -- (AssocL, 9).  Concretely: GHC.Arr.listArray's fill loop tests
    -- @i# ==# n# -# 1#@; without these, @==#@ and @-#@ share precedence 9
    -- and it parses as @(i# ==# n#) -# 1#@ → @isTrue#@ of a nonzero diff →
    -- the loop stops after the first element, dropping the array's last
    -- entry.  Fixities match GHC's primops.txt.pp.
    , ("+#",   (AssocL, 6))
    , ("-#",   (AssocL, 6))
    , ("*#",   (AssocL, 7))
    , ("==#",  (AssocN, 4))
    , ("/=#",  (AssocN, 4))
    , ("<#",   (AssocN, 4))
    , ("<=#",  (AssocN, 4))
    , (">#",   (AssocN, 4))
    , (">=#",  (AssocN, 4))
    , ("+##",  (AssocL, 6))
    , ("-##",  (AssocL, 6))
    , ("*##",  (AssocL, 7))
    , ("/##",  (AssocL, 7))
    , ("**##", (AssocR, 8))
    , ("==##", (AssocN, 4))
    , ("/=##", (AssocN, 4))
    , ("<##",  (AssocN, 4))
    , ("<=##", (AssocN, 4))
    , (">##",  (AssocN, 4))
    , (">=##", (AssocN, 4))
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
        let (precTok, curBeforePrec, curAfterPrec) = skipNewlinesWithStart cur
        case tkKind precTok of
            TkInt n
                | n < 0 || n > 9 -> throwIO (ParseError
                    { peFile = srcName src
                    , peLine = tkLine precTok
                    , peCol  = tkCol precTok
                    , peMsg  = "fixity precedence must be in 0..9, got "
                               <> show n })
                | otherwise -> consumeOps assoc (fromInteger n) acc curAfterPrec
            TkEof -> pure acc
            _
                | tkCol precTok == 1 -> go acc curBeforePrec
                | otherwise          -> consumeOps assoc 9 acc curBeforePrec

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
                    (closeTok, cur3) = nextToken src cur2
                in case (backtickOpName (tkKind idTok), tkKind closeTok) of
                    (Just n, TkBacktick) ->
                        let key = BC.pack "`" <> n <> BC.pack "`"
                        in consumeOps assoc prec (Map.insert key (assoc, prec) acc) cur3
                    _ -> consumeOps assoc prec acc cur2
            _ | Just name <- tokenOpName (tkKind tok) ->
                consumeOps assoc prec (Map.insert name (assoc, prec) acc) cur1
            _ -> go acc cur1

    skipNewlines cur =
        let (t, c) = nextToken src cur in
        case tkKind t of
            TkNewline -> skipNewlines c
            _         -> (t, c)

    skipNewlinesWithStart cur =
        let (t, c) = nextToken src cur in
        case tkKind t of
            TkNewline -> skipNewlinesWithStart c
            _         -> (t, cur, c)

    backtickOpName = \case
        TkIdent n -> Just n
        TkConId n -> Just n
        _         -> Nothing

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
parseBodyExprWithFixity src fx clauses = liftLex $ do
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
parseExprOnly src fx = liftLex $ do
    let end = BC.length (srcBytes src)
        ctx = Ctx src end 0 fx
        cur = startCursor
    (e, _) <- parseExpr ctx cur
    pure e

-- | Like 'parseExprOnly', but additionally requires that the parser
-- consume the entire input. Trailing tokens (including stray operators
-- or garbage after a complete expression) raise a 'ParseError' instead
-- of being silently ignored. Use this in tests that need to assert the
-- parser actually saw the whole snippet rather than stopping early.
parseExprAtEof :: Source -> FixityTable -> IO Expr
parseExprAtEof src fx = liftLex $ do
    let end = BC.length (srcBytes src)
        ctx = Ctx src end 0 fx
        cur = startCursor
    (e, cur1) <- parseExpr ctx cur
    let (tok, _) = nextToken src cur1
    case tkKind tok of
        TkEof -> pure e
        leftover -> throwIO (ParseError (srcName src) (cLine cur1) (cCol cur1)
                ("trailing tokens after expression; first leftover: " <> show leftover))

--------------------------------------------------------------------------------
-- Parsed clauses
--------------------------------------------------------------------------------

type ParsedClause = ([Pat], Rhs)

data Rhs
    = RhsPlain  !Expr
    | RhsGuards ![([Guard], Expr)]

data Guard
    = GuardExpr !Expr
    | GuardPat !Pat !Expr

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
        wrapGuard = \case
            GuardExpr g   -> GuardExpr (wrapWhere g)
            GuardPat p ge -> GuardPat p (wrapWhere ge)
    let rhs' = case rhs of
            RhsPlain e    -> RhsPlain  (wrapWhere e)
            RhsGuards ges -> RhsGuards [(map wrapGuard gs, wrapWhere b) | (gs, b) <- ges]
    pure (pats, rhs')

--------------------------------------------------------------------------------
-- LHS pattern parsing
--------------------------------------------------------------------------------

-- | Parse a single pattern from a pre-located source span.  Used by the
-- pattern-synonym registration path (see "IHC.Scan.scanPatternSynonyms")
-- to materialise the body pattern of a @pattern Name p \<- body@ decl.
parsePatIn :: Source -> FixityTable -> Span -> IO Pat
parsePatIn src fx (start, end) = do
    let ctx  = Ctx src end 0 fx
        (startLine, startCol) = offsetToPos src start
        cur0 = Cursor start startLine startCol
    liftLex $ do
        (p, _) <- parseTopPat ctx cur0
        pure p

parsePatsIn :: Source -> FixityTable -> Span -> IO [Pat]
parsePatsIn src fx (start, end) = do
    let ctx  = Ctx src end 0 fx
        (startLine, startCol) = offsetToPos src start
        cur0 = Cursor start startLine startCol
    -- Distinguish prefix vs infix LHS.  Prefix form `name pat1 pat2 = body`
    -- expects atomic patterns (constructors with args must be parenthesised:
    -- `f (Just x) y = …`).  Infix form `pat1 op pat2 = body` allows full
    -- constructor-application patterns on either side: `Just f <*> m = …`
    -- means @(<*>) (Just f) m = …@, with `Just f` as ONE pattern.  Without
    -- the lookahead below we'd parse @Just f <*> m@ as three atomic
    -- patterns @[Just, f, m]@ and silently drop the constructor argument.
    isInfix <- hasTopLevelInfixOp ctx cur0
    if isInfix
        then loopInfix ctx cur0 []
        else loop ctx cur0 []
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
            -- argument patterns must be skipped. A leading `~` is the
            -- irrefutable-pattern prefix, not an operator separator.
            TkSymOp op | op /= BC.pack "~" -> loop ctx cur1 acc
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

    -- Infix LHS: `pat1 OP pat2 = body` or `pat1 \`fn\` pat2 = body`.
    -- Each side is a full constructor-application pattern (parseTopPat
    -- absorbs ctor args), and the operator/backtick name in the middle
    -- is consumed without being kept as a pattern.
    --
    -- The skipped operator can be either a generic 'TkSymOp' or one of
    -- the dedicated-op tokens carved out by the lexer (==, /=, <, <=,
    -- etc.; see 'isDedicatedInfixOpKind').  Without skipping the
    -- dedicated forms, an instance method whose LHS uses '==' (e.g.
    -- @Status \{ statusCode = a } == ... = ...@) would treat the '==' as
    -- not-a-pattern, stop at the first side, and produce the wrong LHS
    -- pattern list / arity.
    loopInfix ctx cur acc = do
        let (tok, cur1) = nextSig ctx cur
        case tkKind tok of
            TkEof -> pure (reverse acc)
            TkBacktick ->
                case readBacktickName ctx cur1 of
                    Just (_, cur2) ->
                        let (closeTok, cur3) = nextSig ctx cur2
                        in case tkKind closeTok of
                            TkBacktick -> loopInfix ctx cur3 acc
                            _          -> pure (reverse acc)
                    Nothing -> pure (reverse acc)
            TkSymOp op | op /= BC.pack "~" -> loopInfix ctx cur1 acc
            k | isDedicatedInfixOpKind k -> loopInfix ctx cur1 acc
            TkAt ->
                let (peek2, cur2) = nextSig ctx cur1
                in case tkKind peek2 of
                    TkSymOp _ -> loopInfix ctx cur2 acc
                    _         -> loopInfix ctx cur1 acc
            _ | not (startsPat (tkKind tok)) -> pure (reverse acc)
              | otherwise -> do
                (p, cur') <- parseTopPat ctx cur
                loopInfix ctx cur' (p : acc)

-- | True for the dedicated-operator tokens the lexer carves out — the
-- ones that can appear as the infix operator in a function/method LHS
-- (==, /=, <, <=, >, >=, &&, ||, +, ++, *, :, $).  Used by the LHS
-- parser to (a) decide that an LHS uses infix form and (b) skip the
-- operator token between the two pattern arguments.
--
-- Mirrors the dedicated-op cases in 'IHC.Scan.findTopLevelOpBeforeEq'
-- (the scanner that registers the binding under its operator name) so
-- the parser and the scanner agree on which forms count as infix.
isDedicatedInfixOpKind :: TokenKind -> Bool
isDedicatedInfixOpKind k = case k of
    TkEqEq     -> True
    TkNeq      -> True
    TkLt       -> True
    TkLe       -> True
    TkGt       -> True
    TkGe       -> True
    TkAnd      -> True
    TkOr       -> True
    TkPlus     -> True
    TkPlusPlus -> True
    TkStar     -> True
    -- TkMinus IS dedicated infix when scanning a method LHS at depth 0
    -- with preceding patterns (e.g. @I# x - I# y = …@ in Num Int).
    -- Unary-minus in expressions is handled separately by the expression
    -- parser via ENeg.
    TkMinus    -> True
    TkColon    -> True
    TkDollar   -> True
    _          -> False

-- | Look ahead in @cur0..end@ for a top-level (paren-depth 0) infix
-- operator before @=@ or @|@.  Mirrors 'IHC.Scan.findTopLevelOpBeforeEq'
-- but operates on parser tokens.  Returns True for any @TkSymOp@ /
-- @TkBacktick@ / dedicated-op token at depth 0 before the RHS separator.
hasTopLevelInfixOp :: Ctx -> Cursor -> IO Bool
hasTopLevelInfixOp ctx0 cur0 = go cur0 (0 :: Int) False
  where
    -- @sawPat@: at least one pattern-starting token has been seen at
    -- depth 0.  TkMinus alone (no preceding pat) is unary-minus on a
    -- negative literal pattern @-1@, NOT an infix operator.  All other
    -- dedicated infix tokens are unambiguous from token zero so they
    -- don't need the gate.
    go cur depth !sawPat =
        let (tok, cur') = nextSig ctx0 cur in
        case tkKind tok of
            TkEof                       -> pure False
            TkSymOp op | depth == 0
                       , op /= BC.pack "~" -> pure True
            TkSymOp _  | depth == 0     -> go cur' depth sawPat
            TkBacktick | depth == 0     -> pure True
            TkMinus    | depth == 0
                       , sawPat         -> pure True
            TkMinus    | depth == 0     -> go cur' depth sawPat
            k | depth == 0
              , isDedicatedInfixOpKind k -> pure True
            TkLParen                    -> go cur' (depth + 1) sawPat
            TkLBracket                  -> go cur' (depth + 1) sawPat
            TkLBrace                    -> go cur' (depth + 1) sawPat
            TkRParen                    -> go cur' (max 0 (depth - 1)) True
            TkRBracket                  -> go cur' (max 0 (depth - 1)) True
            TkRBrace                    -> go cur' (max 0 (depth - 1)) True
            _ | depth == 0
              , startsPat (tkKind tok)  -> go cur' depth True
            _                           -> go cur' depth sawPat

-- | Look ahead for a top-level (paren-depth 0) /constructor/ operator
-- before @=@ / @|@.  Constructor operators are @:@ and any 'TkSymOp'
-- whose spelling starts with @:@ (Haskell 2010 §2.4 / §4.3.2 @conop@).
--
-- Used to distinguish unparenthesised pattern bindings
--
-- @
--   a :| as = rhs     -- pattern bind (conop)
--   x : xs  = rhs     -- pattern bind (list cons)
-- @
--
-- from ordinary function bindings and variable-operator equations
--
-- @
--   f x     = rhs     -- function
--   x + y   = rhs     -- varop funlhs for (+)
-- @
--
-- Without this, @let a :| as = x@ is mis-parsed as a function named
-- @a@, then fails with "expected `=` or `|`; saw TkSymOp \":|\"",
-- which is the root of the Data.List.NonEmpty lazy @:|@ pattern miss
-- (base's @instance Monad NonEmpty@ uses @where b :| bs = f a@).
hasTopLevelConOpBeforeEq :: Ctx -> Cursor -> Bool
hasTopLevelConOpBeforeEq ctx0 cur0 = go cur0 (0 :: Int)
  where
    go cur !depth =
        let (tok, cur') = nextSig ctx0 cur in
        case tkKind tok of
            TkEof                    -> False
            TkEq     | depth == 0    -> False
            TkBar    | depth == 0    -> False
            TkColon  | depth == 0    -> True
            TkSymOp op
                | depth == 0
                , isConOpSpelling op -> True
            TkLParen                 -> go cur' (depth + 1)
            TkLBracket               -> go cur' (depth + 1)
            TkLBrace                 -> go cur' (depth + 1)
            TkRParen                 -> go cur' (max 0 (depth - 1))
            TkRBracket               -> go cur' (max 0 (depth - 1))
            TkRBrace                 -> go cur' (max 0 (depth - 1))
            _                        -> go cur' depth

    isConOpSpelling op = case BC.uncons op of
        Just (':', _) -> True
        _             -> False

parseRhsIn :: Source -> FixityTable -> Span -> IO Rhs
parseRhsIn src fx (start, end) = do
    let ctx  = Ctx src end 1 fx
        (startLine, startCol) = offsetToPos src start
        cur0 = Cursor start startLine startCol
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
        (gs, cur2) <- parseGuardList ctx cur
        let (eqTok, curEq) = nextSig ctx cur2
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` after guard" eqTok
        (b, cur3) <- parseExpr ctx curEq
        let (sep, cur4) = nextSig ctx cur3
        case tkKind sep of
            TkBar -> parseGuards ctx cur4 ((gs, b) : acc)
            _     -> pure (reverse ((gs, b) : acc))

parseGuardList :: Ctx -> Cursor -> IO ([Guard], Cursor)
parseGuardList ctx cur0 = go cur0 []
  where
    go cur acc = do
        patAttempt <- try (parseTopPat ctx cur) :: IO (Either ParseError (Pat, Cursor))
        case patAttempt of
            Right (pat, curPat) ->
                let (tok, curAfterTok) = nextSig ctx curPat in
                case tkKind tok of
                    TkLArrow -> do
                        (rhs, curRhs) <- parseExpr ctx curAfterTok
                        continue curRhs (GuardPat pat rhs : acc)
                    _ -> parseBoolGuard cur acc
            Left _ ->
                parseBoolGuard cur acc

    parseBoolGuard cur acc = do
        (g, cur1) <- parseExpr ctx cur
        pure (reverse (GuardExpr g : acc), cur1)

    continue cur acc = do
        let (tok, curNext) = nextSig ctx cur
        case tkKind tok of
            TkComma -> go curNext acc
            _       -> pure (reverse acc, cur)

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
            isStrict (PBang _) = True
            isStrict _         = False
            -- Per Haskell Report §3.17.2: `f !x = body` forces x to
            -- WHNF before body runs. We inject `seq <name> ` for each
            -- strict argument; the trivial-pattern fast path otherwise
            -- binds the lambda parameter to an unforced thunk.
            names    = map toName pats
            wrapSeq acc (p, n)
                | isStrict p = EApp (EApp (EVar "seq") (EVar n)) acc
                | otherwise  = acc
            body'    = foldl wrapSeq body (zip pats names)
        in foldr ELam body' names
desugarClauses clauses arity =
    let argNames = [BC.pack ("$a" ++ show i) | i <- [0 .. arity - 1]]
        -- Include the failing argument values in the error so warp-style
        -- "Non-exhaustive patterns in function: [[IS…],[IP…],[IN…]]"
        -- failures name the runtime shape (VInt / NS / W# / …).
        failMsg = stringToConsList
                    ("Non-exhaustive patterns in function: "
                      <> show (map fst clauses)
                      <> " args=")
        showArgs = foldr
            (\a acc -> EApp (EApp (EVar "++")
                                   (EApp (EVar "show") (EVar a)))
                             (EApp (EApp (EVar "++") (stringToConsList " "))
                                   acc))
            (stringToConsList "")
            argNames
        ultimateFail = EApp (EVar "error")
                            (EApp (EApp (EVar "++") failMsg) showArgs)
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

guardChain :: [([Guard], Expr)] -> Expr -> Expr
guardChain []           fb = fb
guardChain ((gs, e):rest) fb =
    guardStep gs e (guardChain rest fb)

guardStep :: [Guard] -> Expr -> Expr -> Expr
guardStep [] body _ = body
guardStep (GuardExpr g : rest) body fb =
    EIf g (guardStep rest body fb) fb
guardStep (GuardPat p scrut : rest) body fb =
    ECase scrut
        [ Alt p (guardStep rest body fb)
        , Alt PWild fb
        ]

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
        PBang inner ->
            -- Per Haskell Report §3.17.2: `f !p = body` forces argName
            -- to WHNF before matching p / running body. matchPatterns
            -- alone would only force when the inner pattern is a
            -- constructor (via the ECase fall-through); for PVar/PWild
            -- it would silently bind to an unforced thunk. Wrap the
            -- continuation in `seq argName _` to introduce the
            -- strictness edge regardless of the inner shape.
            let body' = matchPatterns ((inner, argName) : rest) body fallback
            in EApp (EApp (EVar "seq") (EVar argName)) body'
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
isTrivialPat (PIrref p) = isTrivialPat p
isTrivialPat _        = False

--------------------------------------------------------------------------------
-- Where-clause split
--------------------------------------------------------------------------------

splitOnWhere :: Source -> Span -> (Span, Maybe Span)
splitOnWhere src (start, end) = go cur0 (0 :: Int) [] []
  where
    (startLine, startCol) = offsetToPos src start
    cur0 = Cursor start startLine startCol

    -- @depth@: bracket nesting ((/[/{); a @where@ inside brackets can't
    -- be the binding's where.
    -- @caseStack@: stack of @of@-keyword columns currently open.  A
    -- @where@ whose column is strictly greater than some stacked @of@
    -- column belongs to a case alternative's scope, not the enclosing
    -- binding.  We pop entries as we encounter tokens at a column <=
    -- the stacked @of@ column, indicating we've left that case's
    -- layout block.
    -- @letStack@: stack of @let@-introduced binding columns.  A @let@
    -- (whether top-level @let ... in@ or do-block @let@) opens an
    -- implicit binding-block whose first binding establishes the
    -- column at which subsequent siblings start.  A @where@ whose
    -- column is strictly greater than some stacked let-binding column
    -- belongs to a binding inside that let, not to the enclosing
    -- outer binding.  Without this, a do-block fixture like
    --
    --   main = do
    --     let helper x = inner x
    --           where inner = ...
    --     print (helper 41)
    --
    -- mis-classifies the inner @where@ as @main@'s where and silently
    -- drops @print (helper 41)@.  We track the let-binding column as
    -- the column of the first non-keyword, non-newline token after
    -- the @let@ keyword (handles both single- and multi-line lets).
    go cur depth caseStack letStack
        | cPos cur >= end = ((start, end), Nothing)
        | otherwise =
            let (tok, cur') = nextToken src cur in
            case tkKind tok of
                TkEof    -> ((start, end), Nothing)
                TkWhere | depth == 0
                        , not (inAnyCase (tkCol tok) caseStack)
                        , not (inAnyLet  (tkCol tok) letStack) ->
                    ((start, tkStart tok), Just (tkEnd tok, end))
                TkLParen   -> go cur' (depth + 1) caseStack letStack
                TkRParen   -> go cur' (max 0 (depth - 1)) caseStack letStack
                TkLBracket -> go cur' (depth + 1) caseStack letStack
                TkRBracket -> go cur' (max 0 (depth - 1)) caseStack letStack
                TkOf       -> go cur' depth
                                  (tkCol tok : popOlderCases (tkCol tok) caseStack)
                                  (popOlderLets (tkCol tok) letStack)
                TkLet      ->
                    -- Peek the next significant token to find the
                    -- let-block's binding column.  Newlines and
                    -- comments are skipped via 'nextSigSimple'.
                    let bindCol = case nextSigSimple src cur' of
                            Just (firstTok, _) -> tkCol firstTok
                            Nothing            -> tkCol tok + 1
                    in go cur' depth
                          (popOlderCases (tkCol tok) caseStack)
                          (bindCol : popOlderLets (tkCol tok) letStack)
                _          -> go cur' depth
                                  (popOlderCases (tkCol tok) caseStack)
                                  (popOlderLets (tkCol tok) letStack)

    -- True iff @col@ is still inside any open case block's body.
    inAnyCase :: Int -> [Int] -> Bool
    inAnyCase _ [] = False
    inAnyCase col (topOf : rest)
        | col > topOf = True
        | otherwise   = inAnyCase col rest

    -- True iff @col@ is still inside any open let block (i.e. col is
    -- strictly greater than the let's binding column).  A @where@ at
    -- exactly the binding column belongs to the let's *parent* binding,
    -- not to a let-binding's RHS.
    inAnyLet :: Int -> [Int] -> Bool
    inAnyLet _ [] = False
    inAnyLet col (topLet : rest)
        | col > topLet = True
        | otherwise    = inAnyLet col rest

    -- Drop any case-stack entries whose layout has ended because we
    -- now see a token at column <= the entry's @of@ column.
    popOlderCases :: Int -> [Int] -> [Int]
    popOlderCases col stk
        | col <= 0  = stk
        | otherwise = dropWhile (\topOf -> col <= topOf) stk

    popOlderLets :: Int -> [Int] -> [Int]
    popOlderLets col stk
        | col <= 0  = stk
        | otherwise = dropWhile (\topLet -> col < topLet) stk

    -- Cheap sig-skipper used only inside 'splitOnWhere' (we don't
    -- have a 'Ctx' here so we can't reuse 'nextSig').  Returns Nothing
    -- on EOF.
    nextSigSimple :: Source -> Cursor -> Maybe (Token, Cursor)
    nextSigSimple s c =
        let (t, c') = nextToken s c in
        case tkKind t of
            TkEof     -> Nothing
            TkNewline -> nextSigSimple s c'
            _         -> Just (t, c')
parseBindingsIn :: Source -> FixityTable -> Span -> IO [Bind]
parseBindingsIn src fx (start, end) = do
    -- Seed the lexer cursor with the source's ACTUAL line/col at the
    -- slice's start byte.  Previously hardcoded to @(1, 1)@, which made
    -- 'tkCol' for subsequent tokens relative to the slice rather than
    -- the file — same-column layout checks
    -- ('collectMoreWhereClauses' for multi-clause where-bindings,
    -- and the inner @|@-pipe column comparison for guards) only fired
    -- on the first clause because every later clause sat at a column
    -- relative to the file but compared against a column relative to
    -- the slice.  Symptom: warp's
    -- @foldlChunks f = go where go !a Empty = a ; go !a (Chunk c cs) = ...@
    -- was parsed as a single @go !a Empty = a@ binding, raising
    -- 'PatternMatchFail' on every non-Empty input — which warp's HTTP1
    -- request-body length pass triggered for every incoming request.
    let (startLine, startCol) = offsetToPos src start
        cur0    = Cursor start startLine startCol
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
            rhsCtx = ctx { ctxMinCol = tkCol nameTok }
        case tkKind sepTok of
            TkEq -> do
                (expr, cur4) <- parseExpr rhsCtx cur3
                (rhs, cur5) <- attachWhere rhsCtx (RhsPlain expr) cur4
                pure (name, params, rhs, cur5)
            TkBar -> do
                (branches, cur4) <- parseLetGuardBranches rhsCtx cur3 []
                (rhs, cur5) <- attachWhere rhsCtx (RhsGuards branches) cur4
                pure (name, params, rhs, cur5)
            _ -> parseErr ctx "expected `=` or `|` in binding" sepTok

    attachWhere ctx rhs cur = do
        let (peekWhere, curAfterWhere) = nextSig ctx cur
        case tkKind peekWhere of
            TkWhere -> do
                (binds, curEnd) <- parseTrailingWhere ctx curAfterWhere
                let wrap e = case binds of
                        [] -> e
                        bs -> ELet bs e
                    wrapGuard = \case
                        GuardExpr g   -> GuardExpr (wrap g)
                        GuardPat p ge -> GuardPat p (wrap ge)
                    rhs' = case rhs of
                        RhsPlain e    -> RhsPlain (wrap e)
                        RhsGuards ges -> RhsGuards [(map wrapGuard gs, wrap b) | (gs, b) <- ges]
                pure (rhs', curEnd)
            _ -> pure (rhs, cur)

    parseTrailingWhere ctx cur0 = do
        let (firstTok, cur1) = nextSig ctx cur0
        case tkKind firstTok of
            TkLBrace -> bracedCursor ctx cur1 []
            TkEof    -> pure ([], cur0)
            _        -> layoutCursor ctx (tkCol firstTok) cur0 []

    -- | Collect all variable names bound by a pattern (left-to-right order).
    patVars (PVar n)         = [n]
    patVars (PAs n p)        = n : patVars p
    patVars (PBang p)        = patVars p
    patVars (PIrref p)       = patVars p
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
    --   a    = case $wh0 of { ~(a, _) -> a }
    --   b    = case $wh0 of { ~(_, b) -> b }
    -- Report §3.12: where/let pattern matches are irrefutable.
    parseWherePatBind ctx accLen cur = do
        -- Use 'parseTopPat' so constructor-application LHS patterns
        -- like @BS _ m = bs@ (i.e. PCon with arguments) and infix
        -- @x : xs = ys@ / @a :| as = ne@ are accepted.  'parseSubPat'
        -- only consumes a single atomic pattern token — for @BS _ m = bs@
        -- it consumed @BS@ as a nullary 'PCon "BS" []' and then bailed
        -- with @expected `=`; saw TkUnderscore@.  This had been
        -- silently failing inside 'discoverInModule' (the error was
        -- caught and turned into 'unbound variable Data.ByteString.
        -- Internal.Type.concat' downstream), which was the root
        -- cause keeping the 'Data.ByteString.concat' shim alive
        -- (rule 4).
        (pat, cur2) <- parseTopPat ctx cur
        let (eqTok, cur3) = nextSig ctx cur2
        case tkKind eqTok of
            TkEq -> do
                (rhsE, cur4) <- parseExpr ctx cur3
                let tmpName  = BC.pack ("$wh" ++ show accLen)
                    tmpBind  = (tmpName, rhsE)
                    vars     = patVars pat
                    -- For each bound variable v, produce:
                    --   v = case $tmp of { ~pat -> v }
                    -- Irrefutable so that @where b :| bs = f a@ matches
                    -- Report §3.12 lazy pattern-binding semantics.
                    perVarBind v =
                        ( v
                        , ECase (EVar tmpName)
                            [ Alt (PIrref pat) (EVar v)
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
        (gs, cur1) <- parseGuardList ctx cur
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` after guard expression" eqTok
        (b, cur3) <- parseExpr ctx cur2
        let acc' = acc ++ [(gs, b)]
        let (sep, cur4) = nextSig ctx cur3
        case tkKind sep of
            TkBar -> parseLetGuardBranches ctx cur4 acc'
            _     -> pure (acc', cur3)

    -- Parse a single binding at the current column, collecting all
    -- consecutive same-name clauses so multi-clause where-bindings
    -- (e.g. `f _ [] = ...; f x (y:_) = ...`) are properly desugared.
    -- Returns a list of Bind because a pattern binding expands to multiple binds.
    parseOne ctx bindCol accLen cur = do
        let (peekTok, cur1Peek) = nextSig ctx cur
        case tkKind peekTok of
            -- As-pattern binding: @name @ pat = rhs@ (e.g.
            -- @qr@(q,r) = quotRem n d@ in Integral's @divMod@ default
            -- at @GHC/Internal/Real.hs:282@).  Desugar to two binds:
            --   @name = rhs@
            --   @pat  = name@  (then 'parseWherePatBind' projects each
            --                   bound variable of @pat@ out of @name@).
            TkIdent n
                | (atTok, _) <- nextSig ctx cur1Peek
                , tkKind atTok == TkAt -> do
                    let (_, cur2Peek)   = nextSig ctx cur1Peek    -- consume @
                    (subPat, cur3Peek)  <- parseSubPat ctx cur2Peek
                    let (eqTok, cur4Peek) = nextSig ctx cur3Peek
                    case tkKind eqTok of
                        TkEq -> do
                            (rhsE, cur5Peek) <- parseExpr ctx cur4Peek
                            -- Project the bound vars of subPat out of name.
                            let nameBind = (n, rhsE)
                                vars     = patVars subPat
                                perVarBind v =
                                    ( v
                                    , ECase (EVar n)
                                        [ Alt (PIrref subPat) (EVar v)
                                        , Alt PWild (EApp (EVar "error")
                                                      (stringToConsList
                                                        "Non-exhaustive as-pattern in where"))
                                        ]
                                    )
                                newBinds = nameBind : map perVarBind vars
                            pure (newBinds, cur5Peek)
                        _ -> parseErr ctx
                                "expected `=` after as-pattern in where-binding"
                                eqTok
            -- Unparenthesised infix conop pattern binding in where:
            --   where b :| bs = f a
            -- (base's Monad NonEmpty).  Route to parseWherePatBind before
            -- the function-binding path swallows the leading ident.
            TkIdent _
                | hasTopLevelConOpBeforeEq ctx cur -> do
                    (newBinds, cur') <- parseWherePatBind ctx accLen cur
                    pure (newBinds, cur')
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
            -- @where ?ip = expr@: implicit-param binding inside a where
            -- clause (seen in GHC.Internal.Exception.errorCallWithCallStackException,
            -- which binds @?callStack@).  We parse and discard it: the
            -- enclosing binding's body references the implicit param but
            -- our interpreter doesn't propagate it from where-bindings
            -- (implicit params survive only through let).  Emitting no
            -- Bind is correct because the body doesn't otherwise refer
            -- to @?ip@ as a regular identifier.
            TkImplicitRef _ -> do
                let (_tok, cur1) = nextSig ctx cur     -- consume ?ip
                let (eqTok, cur2) = nextSig ctx cur1
                case tkKind eqTok of
                    TkEq -> pure ()
                    _    -> parseErr ctx "expected `=` after ?ip in where-binding" eqTok
                (_, cur3) <- parseExpr ctx cur2
                pure ([], cur3)
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
    trySkipWhereSig ctx bindCol =
        trySkipSigWith ctx (skipTypeSigBody ctx bindCol)

    braced ctx cur acc
        | cPos cur >= ctxEnd ctx = pure (reverse acc)
        | otherwise = do
            let (nameTok, _) = nextSig ctx cur
            -- Peek the bind column from the current position.
            let bindCol = tkCol nameTok
            -- In an explicitly braced @where { … }@, sigs and bindings are
            -- delimited by @;@/@}@, not by indentation, so use the
            -- braced-aware sig skipper (stops at @;@ or @}@) instead of the
            -- column-aware one used in layout mode.
            mSkip <- trySkipBracedSig ctx cur
            case mSkip of
                Just cur' -> do
                    let (nextTok, curN) = nextSig ctx cur'
                    case tkKind nextTok of
                        TkSemi   -> braced ctx curN acc
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

    bracedCursor ctx cur acc
        | cPos cur >= ctxEnd ctx = pure (reverse acc, cur)
        | otherwise = do
            let (nameTok, _) = nextSig ctx cur
            let bindCol = tkCol nameTok
            -- See note in 'braced' above: braced where-blocks use @;@/@}@
            -- delimiters, so the sig skipper terminates on those.
            mSkip <- trySkipBracedSig ctx cur
            case mSkip of
                Just cur' -> do
                    let (nextTok, curN) = nextSig ctx cur'
                    case tkKind nextTok of
                        TkSemi   -> bracedCursor ctx curN acc
                        TkRBrace -> pure (reverse acc, curN)
                        TkEof    -> pure (reverse acc, cur')
                        _ | cPos cur' < ctxEnd ctx -> bracedCursor ctx cur' acc
                          | otherwise -> pure (reverse acc, cur')
                Nothing -> do
                    (bs, cur') <- parseOne ctx bindCol (length acc) cur
                    let acc' = reverse bs ++ acc
                    let (sep, curN) = nextSig ctx cur'
                    case tkKind sep of
                        TkSemi   -> bracedCursor ctx curN acc'
                        TkRBrace -> pure (reverse acc', curN)
                        TkEof    -> pure (reverse acc', cur')
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

    layoutCursor ctx bindCol cur acc
        | cPos cur >= ctxEnd ctx = pure (reverse acc, cur)
        | otherwise = do
            mSkip <- trySkipWhereSig ctx bindCol cur
            case mSkip of
                Just cur' -> do
                    let (nextTok, _) = nextSig ctx cur'
                    case tkKind nextTok of
                        TkEof -> pure (reverse acc, cur')
                        _ | tkCol nextTok == bindCol && cPos cur' < ctxEnd ctx ->
                               layoutCursor ctx bindCol cur' acc
                          | otherwise ->
                               pure (reverse acc, cur')
                Nothing -> do
                    (bs, cur') <- parseOne ctx bindCol (length acc) cur
                    let acc' = reverse bs ++ acc
                    let (nextTok, _) = nextSig ctx cur'
                    case tkKind nextTok of
                        TkEof -> pure (reverse acc', cur')
                        _ | tkCol nextTok == bindCol && cPos cur' < ctxEnd ctx ->
                               layoutCursor ctx bindCol cur' acc'
                          | otherwise ->
                               pure (reverse acc', cur')

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
    -- Capture an optional trailing @:: type@ annotation as 'ETyApp'
    -- metadata.  The evaluator's 'ETyApp' branch accumulates the type
    -- bytes as a tag on 'VClassMethod', so a nullary class method like
    -- @pure 42 :: Maybe Int@ gets @"Maybe Int"@ on its tag stack and
    -- can dispatch to the right instance.  Non-class values treat
    -- 'ETyApp' as a pass-through (evaluator line 260+).
    let (tok, cur2) = nextSig ctx cur1
    case tkKind tok of
        TkDColon -> do
            cur3 <- skipTypeToBinding ctx cur2
            let src     = ctxSrc ctx
                tyBytes = BC.dropWhile isAsciiSpace
                            (BC.reverse
                                (BC.dropWhile isAsciiSpace
                                    (BC.reverse
                                        (sliceBytes src (cPos cur2, cPos cur3)))))
            if BS.null tyBytes
                then pure (e, cur3)
                else pure (ETyApp e tyBytes, cur3)
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
-- @)@, @]@, @}@ at depth 0, at keywords @in@ / @of@ / @then@ / @else@,
-- at EOF, or at a token whose column is <= @ctxMinCol@ (so do-block
-- layout boundaries terminate the annotation like
-- @let x = 5 :: Int\n    stmt@ → stop at @stmt@, not eat it).
skipTypeToBinding :: Ctx -> Cursor -> IO Cursor
skipTypeToBinding ctx cur0 = go cur0 (0 :: Int) (0 :: Int) (0 :: Int)
  where
    minCol = ctxMinCol ctx
    atLayoutBoundary tok b p c =
        b == 0 && p == 0 && c == 0 && minCol > 0 && tkCol tok <= minCol
    go cur b p c = do
        let (tok, cur') = nextSig ctx cur
        case tkKind tok of
            TkEof -> pure cur
            _ | atLayoutBoundary tok b p c -> pure cur
            TkRParen | b == 0 && p == 0 && c == 0 -> pure cur   -- don't consume
            TkRBracket | b == 0 && p == 0 && c == 0 -> pure cur
            TkRBrace | b == 0 && p == 0 && c == 0 -> pure cur
            TkComma | b == 0 && p == 0 && c == 0 -> pure cur
            -- `..` ends the annotation in an arithmetic sequence like
            -- @[minBound :: StdMethod .. maxBound]@ (http-types' methodArray);
            -- a type annotation never contains a top-level @..@, so this only
            -- fires on the range operator.  Without it the scanner swallows
            -- @StdMethod .. maxBound@ as the type and the range collapses to a
            -- 1-element list.
            TkDotDot | b == 0 && p == 0 && c == 0 -> pure cur
            TkOf    | b == 0 && p == 0 && c == 0 -> pure cur   -- `case e :: T of …`
            TkThen  | b == 0 && p == 0 && c == 0 -> pure cur
            TkElse  | b == 0 && p == 0 && c == 0 -> pure cur
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

-- | Skip the body of a @name :: type@ signature inside an explicitly braced
-- let/where block (@let { x :: T; x = e }@ / @where { x :: T; x = e }@).
-- Called after consuming @::@.  Stops when it sees @;@ or @}@ at depth 0,
-- or EOF.  Used by 'bracedLetBinds' and 'bracedBinds'; the layout-mode
-- variants use 'skipTypeSigBody' which is column-aware instead.
skipTypeSigBodyBraced :: Ctx -> Cursor -> IO Cursor
skipTypeSigBodyBraced ctx cur0 = go cur0 (0 :: Int) (0 :: Int) (0 :: Int)
  where
    go cur b p c = do
        let (tok, cur') = nextSig ctx cur
        case tkKind tok of
            TkEof -> pure cur
            TkSemi   | b == 0 && p == 0 && c == 0 -> pure cur
            TkRBrace | c == 0 && b == 0 && p == 0 -> pure cur  -- closes the block
            TkRParen | p > 0    -> go cur' b (p - 1) c
            TkRParen            -> pure cur
            TkRBracket | b > 0  -> go cur' (b - 1) p c
            TkRBracket          -> pure cur
            TkRBrace | c > 0    -> go cur' b p (c - 1)
            TkLParen   -> go cur' b (p + 1) c
            TkLBracket -> go cur' (b + 1) p c
            TkLBrace   -> go cur' b p (c + 1)
            _          -> go cur' b p c

-- | Detect a @name[, name]* :: type@ signature starting at @cur@ in a
-- braced let/where block.  Returns @Just curAfter@ (positioned just before
-- the @;@ or @}@ that terminates the sig) if a sig was found, otherwise
-- @Nothing@.  Used by 'bracedLetBinds' and 'bracedBinds'.
trySkipBracedSig :: Ctx -> Cursor -> IO (Maybe Cursor)
trySkipBracedSig ctx = trySkipSigWith ctx (skipTypeSigBodyBraced ctx)

-- | Shared @name[, name]* ::@ scanner used by every variant of the
-- "this looks like a signature, not a value binding" check
-- ('trySkipBracedSig' and the layout-mode locals
-- 'trySkipWhereSig' / 'trySkipLetSig' / 'trySkipDoLetSig' wrap this).
-- The caller supplies the body skipper to invoke once @::@ is consumed —
-- column-aware ('skipTypeSigBody') in layout mode, brace-aware
-- ('skipTypeSigBodyBraced') in @{ … }@ mode.
trySkipSigWith :: Ctx -> (Cursor -> IO Cursor) -> Cursor -> IO (Maybe Cursor)
trySkipSigWith ctx skipBody cur = do
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
                        TkDColon -> Just <$> skipBody c'
                        _ -> pure Nothing
            in skipNames cur1
        _ -> pure Nothing

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
            TkBar -> parseMultiWayIf c2 cur4 (([GuardExpr g], b) : acc)
            _     ->
                let branches = reverse (([GuardExpr g], b) : acc)
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
                                     ("Non-exhaustive patterns in lambda: " <> show p)))
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
    pure (ELam n (buildCaseExpr (EVar n) alts), curEnd)

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
        (ss, cur') <- parseStmt ctx cur
        let acc' = reverse ss ++ acc
        let (sep, curN) = nextSig ctx cur'
        case tkKind sep of
            TkSemi   -> bracedStmts curN acc'
            TkRBrace -> pure (reverse acc', curN)
            _        -> parseErr ctx "expected `;` or `}` in do-block" sep

    layoutStmts stmtCol cur acc = do
        let stmtCtx = ctx { ctxMinCol = stmtCol }
        (ss, cur') <- parseStmt stmtCtx cur
        let acc' = reverse ss ++ acc
        let (nextTok, curAfterSep) = nextSig ctx cur'
        case tkKind nextTok of
            TkEof -> pure (reverse acc', cur')
            -- Haskell 2010 §2.7 permits an explicit `;` as statement
            -- separator inside implicit-layout do-blocks, e.g.
            -- `do stmt1; stmt2`.  Without this branch the layout path
            -- only accepted stmts at the same starting column, so the
            -- one-line form `(do putStrLn "a"; pure 1)` stopped after
            -- the first stmt and the parser saw garbage afterwards.
            TkSemi -> layoutStmts stmtCol curAfterSep acc'
            -- `where`, `in`, `of`, `then`, `else` are block-ending keywords:
            -- they cannot be the start of a do-statement even when they happen
            -- to appear at the statement column.  This arises in code like:
            --   action = do
            --     stmt1
            --     stmt2
            --   where
            --     helper = ...
            -- where `where` sits at the same indentation level as the stmts.
            TkWhere | tkCol nextTok == stmtCol -> pure (reverse acc', cur')
            TkIn    | tkCol nextTok == stmtCol -> pure (reverse acc', cur')
            -- Closing brackets at the enclosing stmt column end the
            -- do-block layout (they belong to the surrounding
            -- expression, not the do).  This mirrors how a
            -- @(\x -> do { stmt1\n    stmt2\n}) arg@ ends the do at
            -- the @}@ even when the brace sits at the stmt column.
            TkRParen   -> pure (reverse acc', cur')
            TkRBracket -> pure (reverse acc', cur')
            TkRBrace   -> pure (reverse acc', cur')
            TkComma    -> pure (reverse acc', cur')
            _ | tkCol nextTok == stmtCol -> layoutStmts stmtCol cur' acc'
              | otherwise                -> pure (reverse acc', cur')

    -- | Desugar a list of do-statements.  First try the applicative form;
    -- if that doesn't apply, fall back to the classical monadic chain.
    -- Return EDo directly — evalDo handles all statement types.
    -- The monadicDo desugaring (>>= / >> chains) is only needed for
    -- non-IO monads that don't go through evalDo.  For now, keep
    -- EDo and let the evaluator's direct do-handler run, avoiding
    -- synthetic >>= / >> free vars that trigger class dispatch
    -- cascades during discovery.
    desugarDo :: [Stmt] -> Expr
    desugarDo = EDo

    -- | Standard Haskell do-notation desugaring using >>=/>>/let.
    -- Kept for future use when non-IO monads need desugaring.
    _monadicDo :: [Stmt] -> Expr
    _monadicDo []               = EDo []  -- shouldn't happen; fallback
    _monadicDo [SExpr e]        = e
    _monadicDo [SBind _ e]      = e  -- last stmt can't be bind, but be defensive
    _monadicDo [SBangBind _ e]  = e  -- ditto for !x <- m as final stmt
    _monadicDo [SLet bs]        = ELet bs (EDo [])  -- shouldn't happen
    _monadicDo [SImplicitLet bs] = EImplicitLet bs (EDo [])
    _monadicDo (SExpr e : rest) =
        -- e >> do { rest }
        EApp (EApp (EVar ">>") e) (_monadicDo rest)
    _monadicDo (SBind name e : rest) =
        -- e >>= \name -> do { rest }
        EApp (EApp (EVar ">>=") e) (ELam name (_monadicDo rest))
    _monadicDo (SBangBind name e : rest) =
        -- !name <- e ;  rest   ==>   e >>= \name -> seq name (do { rest })
        -- Per Haskell Report §3.17.2 + GHC BangPatterns: the bound result
        -- is forced to WHNF before the rest of the do-block runs.
        EApp (EApp (EVar ">>=") e)
             (ELam name
                 (EApp (EApp (EVar "seq") (EVar name)) (_monadicDo rest)))
    _monadicDo (SLet bs : rest) =
        -- let bs in do { rest }
        ELet bs (_monadicDo rest)
    _monadicDo (SImplicitLet bs : rest) =
        -- let ?x = e in do { rest }
        EImplicitLet bs (_monadicDo rest)

    -- | If the do-block matches the applicative pattern, return its
    -- applicative desugaring.  See the header comment on 'parseDo' for the
    -- full set of conditions.
    _tryApplicativeDo :: [Stmt] -> Maybe Expr
    _tryApplicativeDo stmts = do
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
                all (`notElem` seen) (_exprFreeVars rhs)
                && independent (n : seen) rs

    -- | Free variables of an 'Expr' (names referenced via 'EVar' that
    -- aren't shadowed by a lambda, let, or pattern binding within).  Kept
    -- local so the parser doesn't depend on the scheduler; the version in
    -- 'IHC.Scheduler' covers more constructors but this subset is enough
    -- for ApplicativeDo's independence check.
    _exprFreeVars :: Expr -> [Name]
    _exprFreeVars = fv []
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
            EQuasiQuote n _
                | n `elem` bound -> []
                | otherwise      -> [n]
            ELabel  _          -> []
            ETyApp e _         -> fv bound e
            ETypedMethod {}    -> []
            EGuardFail         -> []

        fvStmts _     []                  = []
        fvStmts bound (SExpr e   : rest)  = fv bound e ++ fvStmts bound rest
        fvStmts bound (SBind n e : rest)  = fv bound e ++ fvStmts (n : bound) rest
        fvStmts bound (SBangBind n e : rest) = fv bound e ++ fvStmts (n : bound) rest
        fvStmts bound (SLet bs   : rest)  =
            let names  = map fst bs
                bound' = names ++ bound
            in concatMap (fv bound' . snd) bs ++ fvStmts bound' rest
        fvStmts bound (SImplicitLet bs : rest) =
            concatMap (fv bound . snd) bs ++ fvStmts bound rest

        fvAlt bound (Alt p e) = fv (patBound p ++ bound) e

        patBound :: Pat -> [Name]
        patBound (PVar n)        = [n]
        patBound (PCon _ ps)     = concatMap patBound ps
        patBound (PAs n p)       = n : patBound p
        patBound (PBang p)       = patBound p
        patBound (PIrref p)      = patBound p
        patBound (PTuple ps)     = concatMap patBound ps
        patBound (PRecord _ fps) = concatMap (patBound . snd) fps
        patBound (PRecordWild _) = []
        patBound (PView _ p)     = patBound p
        patBound _               = []

parseStmt :: Ctx -> Cursor -> IO ([Stmt], Cursor)
parseStmt ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkLet -> parseDoLet ctx cur1
        TkIdent name -> do
            let (peek, cur2) = nextSig ctx cur1
            case tkKind peek of
                TkLArrow -> do
                    (e, cur3) <- parseExpr ctx cur2
                    pure ([SBind name e], cur3)
                TkAt | hasTopLevelBindArrow cur0 -> parseDoPatBindStmt
                _ -> do
                    (e, cur') <- parseExpr ctx cur0
                    pure ([SExpr e], cur')
        -- `_ <- expr` — wildcard bind; discard the result.
        TkUnderscore -> do
            let (peek, cur2) = nextSig ctx cur1
            case tkKind peek of
                TkLArrow -> do
                    (e, cur3) <- parseExpr ctx cur2
                    pure ([SBind (BC.pack "_") e], cur3)
                _ -> do
                    (e, cur') <- parseExpr ctx cur0
                    pure ([SExpr e], cur')
        _ | startsPat (tkKind tok) && hasTopLevelBindArrow cur0 ->
            parseDoPatBindStmt
        _ -> do
            (e, cur') <- parseExpr ctx cur0
            pure ([SExpr e], cur')
  where
    parseDoPatBindStmt = do
        (pat, curPat) <- parseTopPat ctx cur0
        let (arrTok, curAfterArr) = nextSig ctx curPat
        case tkKind arrTok of
            TkLArrow -> do
                (e, cur') <- parseExpr ctx curAfterArr
                pure (lowerDoPatBind pat e, cur')
            _ -> parseErr ctx "expected `<-` after do pattern" arrTok

    hasTopLevelBindArrow cur = go cur (0 :: Int) (0 :: Int) (0 :: Int)
      where
        go c par br brace =
            let (t, c') = nextSig ctx c
            in case tkKind t of
                TkEof -> False
                _ | ctxMinCol ctx > 0
                  , par == 0 && br == 0 && brace == 0
                  , cPos c /= cPos cur
                  , tkCol t <= ctxMinCol ctx -> False
                TkLArrow | par == 0 && br == 0 && brace == 0 -> True
                TkDo | par == 0 && br == 0 && brace == 0 -> False
                TkIf | par == 0 && br == 0 && brace == 0 -> False
                TkLet | par == 0 && br == 0 && brace == 0 -> False
                TkCase | par == 0 && br == 0 && brace == 0 -> False
                TkBackslash | par == 0 && br == 0 && brace == 0 -> False
                TkDollar | par == 0 && br == 0 && brace == 0 -> False
                TkEq | par == 0 && br == 0 && brace == 0 -> False
                TkSemi | par == 0 && br == 0 && brace == 0 -> False
                TkRBrace | par == 0 && br == 0 && brace == 0 -> False
                TkLParen   -> go c' (par + 1) br brace
                TkRParen   -> go c' (max 0 (par - 1)) br brace
                TkLBracket -> go c' par (br + 1) brace
                TkRBracket -> go c' par (max 0 (br - 1)) brace
                TkLBrace   -> go c' par br (brace + 1)
                TkRBrace   -> go c' par br (max 0 (brace - 1))
                _          -> go c' par br brace

    lowerDoPatBind pat action =
        case pat of
            PVar n -> [SBind n action]
            PWild  -> [SBind (BC.pack "_") action]
            PBang (PVar n) -> [SBangBind n action]
            _ ->
                let tmpName = BC.pack ("$doBindPat" <> show (cPos cur0))
                    vars = nubBSLocal (patVars pat)
                    project v =
                        ( v
                        , ECase (EVar tmpName)
                            [ Alt pat (EVar v)
                            , Alt PWild (EApp (EVar "error")
                                          (stringToConsList
                                            ("Non-exhaustive pattern in do binding: "
                                             <> show pat)))
                            ]
                        )
                in [SBind tmpName action, SLet (map project vars)]

    patVars (PVar n)         = [n]
    patVars (PAs n p)        = n : patVars p
    patVars (PBang p)        = patVars p
    patVars (PIrref p)       = patVars p
    patVars (PTuple ps)      = concatMap patVars ps
    patVars (PCon _ ps)      = concatMap patVars ps
    patVars (PRecord _ fps)  = concatMap (patVars . snd) fps
    patVars (PRecordWild _)  = []
    patVars (PView _ p)      = patVars p
    patVars PWild            = []
    patVars (PLit _)         = []

    nubBSLocal = go []
      where
        go _ [] = []
        go seen (x:xs)
            | x `elem` seen = go seen xs
            | otherwise     = x : go (x : seen) xs

parseDoLet :: Ctx -> Cursor -> IO ([Stmt], Cursor)
parseDoLet ctx cur0 = do
    let (firstTok, curAfter) = nextSig ctx cur0
    case tkKind firstTok of
        -- `let ?x = e` inside do — implicit-param binding that scopes
        -- to the remainder of the do-block via 'EImplicitLet'.
        TkImplicitRef _ -> do
            (iBinds, curEnd) <- parseImplicitDoBinds (tkCol firstTok) cur0 []
            pure ([SImplicitLet iBinds], curEnd)
        TkLBrace -> do
            let (peek, _) = nextSig ctx curAfter
            case tkKind peek of
                TkImplicitRef _ -> do
                    (iBinds, curEnd) <- parseImplicitBracedBinds curAfter []
                    pure ([SImplicitLet iBinds], curEnd)
                _ -> do
                    (binds, curEnd) <- bracedBinds curAfter []
                    pure (doLetStmts binds, curEnd)
        -- Per Haskell Report §3.17.2 + GHC BangPatterns: `let !x = e`
        -- inside a do-block must force e to WHNF before the rest of the
        -- block runs. Lower the simple single-binding form to
        -- @!x <- pure e@, which we already implement via SBangBind.
        -- Multi-binding lets (`let !x = e1; y = e2`) and complex bang
        -- patterns (`let !(a, b) = e`) fall through to the layout path
        -- below — covered separately by A.1/A.5 follow-ups.
        TkBang -> do
            let (peekId, curId) = nextSig ctx curAfter
            case tkKind peekId of
                TkIdent n -> do
                    let (peekEq, curEq) = nextSig ctx curId
                    case tkKind peekEq of
                        TkEq -> do
                            -- Peek past the RHS to confirm this is a single
                            -- binding (no follow-up at the same column).
                            let bindCol = tkCol firstTok
                                rhsCtx  = ctx { ctxMinCol = bindCol }
                            (rhs, cur3) <- parseExpr rhsCtx curEq
                            let (peekAfter, _) = nextSig ctx cur3
                                isSingle = tkCol peekAfter /= bindCol
                                            || tkKind peekAfter == TkEof
                            if isSingle
                                then pure ([SBangBind n
                                              (EApp (EVar "pure") rhs)], cur3)
                                else do
                                    -- Multi-binding let; fall back to layout.
                                    (binds, curEnd) <- layoutBinds bindCol cur0 []
                                    pure (doLetStmts binds, curEnd)
                        _ -> fallbackLayout firstTok
                _ -> fallbackLayout firstTok
        _ -> fallbackLayout firstTok
  where
    fallbackLayout firstTok =
        let bindCol = tkCol firstTok
        in do
            (binds, curEnd) <- layoutBinds bindCol cur0 []
            pure (doLetStmts binds, curEnd)

    doLetStmts binds = [SLet binds]

    parseImplicitDoBinds bindCol cur acc = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkImplicitRef n -> pure n
            _ -> parseErr ctx "expected `?name` in implicit let-binding" nameTok
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` in implicit let-binding" eqTok
        (e, cur3) <- parseExpr (ctx { ctxMinCol = bindCol }) cur2
        let (peek, _) = nextSig ctx cur3
        if tkCol peek == bindCol && tkKind peek /= TkEof
            then case tkKind peek of
                TkImplicitRef _ -> parseImplicitDoBinds bindCol cur3 ((name, e) : acc)
                _ -> pure (reverse ((name, e) : acc), cur3)
            else pure (reverse ((name, e) : acc), cur3)

    parseImplicitBracedBinds cur acc = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkImplicitRef n -> pure n
            _ -> parseErr ctx "expected `?name` in implicit let-binding" nameTok
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` in implicit let-binding" eqTok
        (e, cur3) <- parseExpr ctx cur2
        let (sep, curN) = nextSig ctx cur3
        case tkKind sep of
            TkSemi   -> parseImplicitBracedBinds curN ((name, e) : acc)
            TkRBrace -> pure (reverse ((name, e) : acc), curN)
            _        -> parseErr ctx "expected `;` or `}` in implicit let-block" sep

    -- | Try to detect and skip a type sig @name[, name]* :: type@ at the
    -- current position. Returns @Just curAfter@ on success, @Nothing@ if
    -- this looks like a value binding.
    trySkipDoLetSig bindCol = trySkipSigWith ctx (skipTypeSigBody ctx bindCol)

    -- | Collect every variable bound by a pattern (left-to-right order).
    doLetPatVars (PVar n)        = [n]
    doLetPatVars (PAs n p)       = n : doLetPatVars p
    doLetPatVars (PBang p)       = doLetPatVars p
    doLetPatVars (PIrref p)      = doLetPatVars p
    doLetPatVars (PTuple ps)     = concatMap doLetPatVars ps
    doLetPatVars (PCon _ ps)     = concatMap doLetPatVars ps
    doLetPatVars (PRecord _ fps) = concatMap (doLetPatVars . snd) fps
    doLetPatVars (PRecordWild _) = []
    doLetPatVars (PView _ p)     = doLetPatVars p
    doLetPatVars PWild           = []
    doLetPatVars (PLit _)        = []

    -- | Parse `pat = rhs` inside a do-block let and desugar into a list
    -- of normal bindings: a `$doPatN` temp for the RHS, plus one
    -- per bound variable that projects via `case $doPatN of pat -> v`.
    parseDoLetPatBind n cur = do
        let (firstTok, _) = nextSig ctx cur
            bindCol = tkCol firstTok
        (pat, cur1) <- parseTopPat ctx cur
        let rhsCtx = ctx { ctxMinCol = bindCol }
            (sepTok, cur2) = nextSig ctx cur1
        (rhsE, cur3) <- case tkKind sepTok of
            TkEq -> parseExpr rhsCtx cur2
            TkBar -> do
                (branches, cur3') <- parseDoLetGuardBranches rhsCtx cur2 []
                pure (desugarClauses [([], RhsGuards branches)] 0, cur3')
            _ -> parseErr ctx "expected `=` or `|` in pattern let-binding" sepTok
        let tmpName = BC.pack ("$doPat" ++ show n)
            strictName = BC.pack ("$doPatStrict" ++ show n)
            vars    = doLetPatVars pat
            -- Report §3.12: let patterns are irrefutable unless bang-strict.
            matchPat'
                | isStrictDoLetPat pat = pat
                | otherwise            = PIrref pat
            perVar v =
                ( v
                , ECase (EVar tmpName)
                    [ Alt matchPat' (EVar v)
                    , Alt PWild (EApp (EVar "error")
                                  (stringToConsList
                                    "Non-exhaustive pattern in let"))
                    ]
                )
            strictBind =
                ( strictName
                , ECase (EVar tmpName)
                    [ Alt pat (EVar "()")
                    , Alt PWild (EApp (EVar "error")
                                  (stringToConsList
                                    "Non-exhaustive pattern in strict let"))
                    ]
                )
            newBinds =
                (tmpName, rhsE)
                : map perVar vars
                ++ [strictBind | isStrictDoLetPat pat]
        pure (newBinds, cur3)

    isStrictDoLetPat (PBang _) = True
    isStrictDoLetPat _         = False

    parseDoLetClauseRaw bindCol cur = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkIdent n -> pure n
            _         -> parseErr ctx "expected identifier in let-binding" nameTok
        (params, cur2) <- collectLetParams ctx cur1 []
        let (sepTok, cur3) = nextSig ctx cur2
            rhsCtx = ctx { ctxMinCol = bindCol }
        (rhs0, cur4) <- case tkKind sepTok of
            TkEq -> do
                (expr, cur4') <- parseExpr rhsCtx cur3
                pure (RhsPlain expr, cur4')
            TkBar -> do
                (branches, cur4') <- parseDoLetGuardBranches rhsCtx cur3 []
                pure (RhsGuards branches, cur4')
            _     -> parseErr ctx "expected `=` or `|` in let-binding" sepTok
        -- Consume a trailing @where@ on this binding's RHS so that
        -- @do { let f x = body where helper = ... ; rest }@ parses
        -- correctly.  Without this, @parseDoLet@'s @layoutBinds@ stops
        -- at the @where@ token (it's not at @bindCol@), and
        -- @parseDo@'s @layoutStmts@ then ALSO stops at @where@ (it's
        -- not at the do-stmt column), so the where binds AND the next
        -- do-stmt get silently dropped.  Note that 'splitOnWhere' has
        -- already excluded this where from the outer binding's range
        -- (its 'inAnyLet' check sees it nested inside the let), so the
        -- enclosing 'ctx' covers the where block end-to-end.
        (rhs, curAfter) <- attachDoLetWhere rhs0 cur4
        pure (name, params, rhs, curAfter)

    attachDoLetWhere rhs cur = do
        let (peekWhere, curAfterWhere) = nextSig ctx cur
        case tkKind peekWhere of
            TkWhere -> do
                let whereTokCol = tkCol peekWhere
                    (whereEndPos, curEnd) = findWhereBlockEnd whereTokCol curAfterWhere
                if cPos curAfterWhere >= whereEndPos
                    then pure (rhs, cur)
                    else do
                        binds <- parseBindingsIn (ctxSrc ctx) (ctxFixity ctx)
                                                 (cPos curAfterWhere, whereEndPos)
                        pure (wrapWhereBinds binds rhs, curEnd)
            _ -> pure (rhs, cur)


    -- Walk forward from a position just after @where@ and return both
    -- the byte offset at which the where-block ends and a 'Cursor'
    -- positioned there with correct line/col (the lexer needs accurate
    -- line/col on the cursor passed to subsequent 'nextToken' calls,
    -- otherwise downstream 'tkCol' checks compare against stale columns
    -- from the cursor's previous position).  Block ends at the first
    -- non-newline token whose column is <= @whereTokCol@.
    findWhereBlockEnd whereTokCol startCur = go startCur
      where
        go c
            | cPos c >= ctxEnd ctx = (ctxEnd ctx, c)
            | otherwise =
                let (t, c') = nextToken (ctxSrc ctx) c in
                case tkKind t of
                    TkEof     -> (cPos c, c)
                    TkNewline -> go c'
                    _ | tkCol t <= whereTokCol ->
                          -- Build a cursor at the start of @t@ with
                          -- the lexer-reported line/col for that
                          -- position.  Using @c@ (the cursor before
                          -- this nextToken call) would carry stale
                          -- line/col from before whitespace was
                          -- skipped.
                          (tkStart t, Cursor (tkStart t) (tkLine t) (tkCol t))
                      | otherwise              -> go c'

    wrapWhereBinds [] r = r
    wrapWhereBinds bs r = case r of
        RhsPlain e    -> RhsPlain (ELet bs e)
        RhsGuards ges -> RhsGuards
            [ ( map (wrapGuardBinds bs) gs
              , ELet bs b
              )
            | (gs, b) <- ges
            ]

    wrapGuardBinds bs (GuardExpr g)   = GuardExpr (ELet bs g)
    wrapGuardBinds bs (GuardPat p ge) = GuardPat p (ELet bs ge)

    collectMoreDoLetClauses bindCol name cur acc = do
        let (peek, _) = nextSig ctx cur
        case tkKind peek of
            TkIdent n | tkCol peek == bindCol && n == name -> do
                (_, params, rhs, cur') <- parseDoLetClauseRaw bindCol cur
                collectMoreDoLetClauses bindCol name cur' (acc ++ [(params, rhs)])
            _ -> pure (acc, cur)

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
                case tkKind nameTok of
                    TkIdent n
                        | hasTopLevelConOpBeforeEq ctx cur -> do
                            -- Unparenthesised infix conop pattern in do-let:
                            --   let a :| as = ne
                            (patBinds, cur4) <- parseDoLetPatBind (length acc) cur
                            let (peek, _) = nextSig ctx cur4
                                acc' = reverse patBinds ++ acc
                            if tkCol peek == bindCol && tkKind peek /= TkEof
                                then layoutBinds bindCol cur4 acc'
                                else pure (reverse acc', cur4)
                        | otherwise -> do
                        let _ = cur1
                        (_, params0, rhs0, cur4a) <- parseDoLetClauseRaw bindCol cur
                        (moreClauses, cur4) <- collectMoreDoLetClauses bindCol n cur4a []
                        let clauses = (params0, rhs0) : moreClauses
                            e = desugarClauses clauses (length params0)
                            bind = (n, e)
                        let (peek, _) = nextSig ctx cur4
                        if tkCol peek == bindCol && tkKind peek /= TkEof
                            then layoutBinds bindCol cur4 (bind : acc)
                            else pure (reverse (bind : acc), cur4)
                    k | startsPat k -> do
                        -- Pattern binding in do-block: `let (v, s) = rhs`
                        -- desugars to a temp binding plus one per bound var.
                        (patBinds, cur4) <- parseDoLetPatBind (length acc) cur
                        let (peek, _) = nextSig ctx cur4
                            acc' = reverse patBinds ++ acc
                        if tkCol peek == bindCol && tkKind peek /= TkEof
                            then layoutBinds bindCol cur4 acc'
                            else pure (reverse acc', cur4)
                      | otherwise -> parseErr ctx "expected identifier or pattern after `let`" nameTok

    bracedBinds cur acc = do
        -- Skip a @name :: type@ signature line; the interpreter delays
        -- type-checking, so the sig is a no-op.  Recognised inside
        -- @do { let { … } … }@.
        mSkip <- trySkipBracedSig ctx cur
        case mSkip of
            Just cur' -> do
                let (sep, curN) = nextSig ctx cur'
                case tkKind sep of
                    TkSemi   -> bracedBinds curN acc
                    TkRBrace -> pure (reverse acc, curN)
                    _        -> parseErr ctx "expected `;` or `}` in let-block" sep
            Nothing -> do
                let (nameTok, _) = nextSig ctx cur
                case tkKind nameTok of
                    k | startsPat k, not (isIdent k) -> do
                        (patBinds, cur4) <- parseDoLetPatBind (length acc) cur
                        let (sep, curN) = nextSig ctx cur4
                            acc' = reverse patBinds ++ acc
                        case tkKind sep of
                            TkSemi   -> bracedBinds curN acc'
                            TkRBrace -> pure (reverse acc', curN)
                            _        -> parseErr ctx "expected `;` or `}` in let-block" sep
                    _ -> bracedIdentBind cur acc

    isIdent (TkIdent _) = True
    isIdent _           = False

    bracedIdentBind cur acc = do
        let (nameTok, cur1) = nextSig ctx cur
        -- Unparenthesised infix conop pattern: `{ a :| as = ne; … }`
        case tkKind nameTok of
            TkIdent _
                | hasTopLevelConOpBeforeEq ctx cur -> do
                    (patBinds, cur4) <- parseDoLetPatBind (length acc) cur
                    let (sep, curN) = nextSig ctx cur4
                        acc' = reverse patBinds ++ acc
                    case tkKind sep of
                        TkSemi   -> bracedBinds curN acc'
                        TkRBrace -> pure (reverse acc', curN)
                        _        -> parseErr ctx "expected `;` or `}` in let-block" sep
            _ -> do
                name <- case tkKind nameTok of
                    TkIdent n -> pure n
                    _         -> parseErr ctx "expected identifier in let-binding" nameTok
                (params, cur2) <- collectLetParams ctx cur1 []
                let (sepTok, cur3) = nextSig ctx cur2
                (e, cur4) <- case tkKind sepTok of
                    TkEq -> do
                        (rhs, cur4') <- parseExpr ctx cur3
                        pure (wrapParams params rhs, cur4')
                    TkBar -> do
                        (branches, cur4') <- parseDoLetGuardBranches ctx cur3 []
                        let rhs = desugarClauses
                                    [(params, RhsGuards branches)]
                                    (length params)
                        pure (rhs, cur4')
                    _ -> parseErr ctx "expected `=` or `|` in let-binding" sepTok
                let (sep, curN) = nextSig ctx cur4
                case tkKind sep of
                    TkSemi   -> bracedBinds curN ((name, e) : acc)
                    TkRBrace -> pure (reverse ((name, e) : acc), curN)
                    _        -> parseErr ctx "expected `;` or `}` in let-block" sep

    parseDoLetGuardBranches ctx cur acc = do
        (gs, cur1) <- parseGuardList ctx cur
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` after guard in let" eqTok
        (b, cur3) <- parseExpr ctx cur2
        let acc' = acc ++ [(gs, b)]
        let (sep, cur4) = nextSig ctx cur3
        case tkKind sep of
            TkBar -> parseDoLetGuardBranches ctx cur4 acc'
            _     -> pure (acc', cur3)

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
    -- Irrefutable lambda parameter `\ ~p -> body`: matchPat (PIrref p)
    -- always succeeds, binding each var of p to a thunk that re-attempts
    -- the match on force. Keep the case-wrap intact (matchPat handles
    -- the always-match contract); the PWild fallback is unreachable.
    wrap p         e =
        let n = "$p" in
        ELam n (ECase (EVar n)
                 [ Alt p e
                 , Alt PWild (EApp (EVar "error")
                               (stringToConsList
                                   ("Non-exhaustive patterns in let: " <> show p)))
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
        -- Desugar all items into one recursive let group. Pattern-bound
        -- variables must be in scope for sibling bindings too, e.g.
        --   let (a, b) = rhs
        --       c = b
        --   in c
        -- so expose each variable as its own lazy projection binding instead
        -- of wrapping only the final body in a case.
        let (normalBinds, body1) = foldl desugarItem ([], body0) (reverse items)
        pure (if null normalBinds then body1 else ELet normalBinds body1, curBody)
      where
        desugarItem (normalAcc, bodyAcc) (Left (n, e)) =
            ((n, e) : normalAcc, bodyAcc)
        desugarItem (normalAcc, bodyAcc) (Right (pat, rhsE)) =
            -- let (a, b) = rhs
            -- desugars to:
            --   let $patN = rhs
            --       a = case $patN of { ~(a, _) -> a }
            --       b = case $patN of { ~(_, b) -> b }
            -- Haskell Report §3.12: pattern matches in let/where are always
            -- irrefutable (lazy).  Wrap non-bang patterns in 'PIrref' so
            -- the match is deferred until a bound variable is forced —
            -- matching base's @where b :| bs = f a@ in Monad NonEmpty.
            -- For a strict pattern binding (`let !pat = rhs`), also wrap
            -- the body in a case on the shared temporary.  The projection
            -- bindings stay in the recursive let group so sibling RHSs can
            -- still refer to the pattern variables, but the body now has the
            -- strictness edge that source IO/state loops rely on.
            let tmpName = BC.pack ("$pat" ++ show (length normalAcc))
                vars = letPatVars pat
                matchPat'
                    | isStrictLetPat pat = pat
                    | otherwise          = PIrref pat
                perVarBind v =
                    ( v
                    , ECase (EVar tmpName)
                        [ Alt matchPat' (EVar v)
                        , Alt PWild (EApp (EVar "error")
                                      (stringToConsList
                                        "Non-exhaustive pattern in let"))
                        ]
                    )
                bodyAcc'
                    | isStrictLetPat pat =
                        ECase (EVar tmpName)
                            [ Alt pat bodyAcc
                            , Alt PWild (EApp (EVar "error")
                                          (stringToConsList
                                            "Non-exhaustive pattern in strict let"))
                            ]
                    | otherwise = bodyAcc
            in ((tmpName, rhsE) : map perVarBind vars ++ normalAcc, bodyAcc')

        isStrictLetPat (PBang _) = True
        isStrictLetPat _         = False

        letPatVars (PVar n)        = [n]
        letPatVars (PAs n p)       = n : letPatVars p
        letPatVars (PBang p)       = letPatVars p
        letPatVars (PIrref p)      = letPatVars p
        letPatVars (PTuple ps)     = concatMap letPatVars ps
        letPatVars (PCon _ ps)     = concatMap letPatVars ps
        letPatVars (PRecord _ fps) = concatMap (letPatVars . snd) fps
        letPatVars (PRecordWild _) = []
        letPatVars (PView _ p)     = letPatVars p
        letPatVars PWild           = []
        letPatVars (PLit _)        = []

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
    trySkipLetSig bindCol = trySkipSigWith ctx (skipTypeSigBody ctx bindCol)

    -- Parse one let-binding (name + params + = or | guards + body).
    -- @bindCol@ is the column at which every binding in the enclosing let-block
    -- starts; it is used as @ctxMinCol@ while parsing the RHS so the expression
    -- parser stops when it sees the next binding's identifier at @bindCol@
    -- instead of greedily eating into the next binding.  Without this, a
    -- multi-line let like @let x = expr1\n    y = expr2 in …@ consumed @y@
    -- as a trailing argument to @expr1@.
    -- Handles:
    --   name = expr
    --   name params = expr
    --   name params | guard = expr | guard = expr
    --   (pat, ...) = expr            (pattern binding, returned as Right)
    --   a :| as = expr / x:xs = expr  (infix conop pattern binding)
    parseOneLetItem bindCol cur = do
        let rhsCtx = ctx { ctxMinCol = max (ctxMinCol ctx) bindCol }
        let (nameTok, cur1) = nextSig ctx cur
        case tkKind nameTok of
            TkIdent n
                -- Unparenthesised infix conop pattern: `a :| as = rhs`.
                -- Must win over the function-binding path, else we treat
                -- `a` as the function name and choke on the `:|`.
                | hasTopLevelConOpBeforeEq ctx cur -> do
                    (pat, cur2) <- parseTopPat ctx cur
                    let (eqTok, cur3) = nextSig ctx cur2
                    case tkKind eqTok of
                        TkEq -> do
                            (e, cur4) <- parseExpr rhsCtx cur3
                            pure (Right (pat, e), cur4)
                        _ -> parseErr ctx "expected `=` in pattern let-binding" eqTok
                | otherwise -> do
                (params, cur2) <- collectLetParams ctx cur1 []
                let (sepTok, cur3) = nextSig ctx cur2
                case tkKind sepTok of
                    TkEq -> do
                        (e, cur4) <- parseExpr rhsCtx cur3
                        pure (Left (n, wrapParams params e), cur4)
                    TkBar -> do
                        (branches, cur4) <- parseLetGuardBranches rhsCtx cur3 []
                        let e = desugarClauses [(params, RhsGuards branches)] (length params)
                        pure (Left (n, e), cur4)
                    _ -> parseErr ctx "expected `=` or `|` in let-binding" sepTok
            _ | startsPat (tkKind nameTok) -> do
                -- Pattern binding: (a, b) = expr, Con x = expr, !x = expr, etc.
                -- Use parseTopPat so applied constructors (e.g. `Foo a b`) are
                -- collected into a single PCon instead of stopping at the bare
                -- constructor name — IHP's @let QueryBuilder sq = ...@ idiom.
                (pat, cur2) <- parseTopPat ctx cur
                let (eqTok, cur3) = nextSig ctx cur2
                case tkKind eqTok of
                    TkEq -> do
                        (e, cur4) <- parseExpr rhsCtx cur3
                        pure (Right (pat, e), cur4)
                    _ -> parseErr ctx "expected `=` in pattern let-binding" eqTok
              | otherwise -> parseErr ctx "expected identifier or pattern after `let`" nameTok

    -- Parse `| guard = expr` branches, returning when no more `|` is seen.
    parseLetGuardBranches ctx cur acc = do
        (gs, cur1) <- parseGuardList ctx cur
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` after guard in let" eqTok
        (b, cur3) <- parseExpr ctx cur2
        let acc' = acc ++ [(gs, b)]
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
                (item, cur') <- parseOneLetItem bindCol cur
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
        -- Skip a @name :: type@ signature line (no-op for the interpreter,
        -- which delays type-checking).  Recognised inside @let { … }@.
        mSkip <- trySkipBracedSig ctx cur
        case mSkip of
            Just cur' -> do
                let (sep, curN) = nextSig ctx cur'
                case tkKind sep of
                    TkSemi   -> bracedLetBinds curN acc
                    TkRBrace -> pure (reverse acc, curN)
                    _        -> parseErr ctx "expected `;` or `}` in let-block" sep
            Nothing -> do
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
        _        ->
            -- Layout mode: collect same-column ?x = expr bindings.  The
            -- layout column is the column of the first ?ref.  Each RHS is
            -- parsed with @ctxMinCol = bindCol@ so the expression parser
            -- stops at the next binding instead of eating it.
            let bindCol = tkCol firstTok
            in layoutIPBinds bindCol cur0 []
    let (inTok, curIn) = nextSig ctx curEnd
    case tkKind inTok of
        TkIn -> pure ()
        _    -> parseErr ctx "expected `in` after implicit let" inTok
    (body, curBody) <- parseExpr ctx curIn
    pure (EImplicitLet iBinds body, curBody)
  where
    parseOneIPBind :: Int -> Cursor -> IO ((Name, Expr), Cursor)
    parseOneIPBind bindCol cur = do
        let (nameTok, cur1) = nextSig ctx cur
        name <- case tkKind nameTok of
            TkImplicitRef n -> pure n
            _               -> parseErr ctx "expected `?name` in implicit let" nameTok
        let (eqTok, cur2) = nextSig ctx cur1
        case tkKind eqTok of
            TkEq -> pure ()
            _    -> parseErr ctx "expected `=` in implicit let" eqTok
        let rhsCtx = ctx { ctxMinCol = max (ctxMinCol ctx) bindCol }
        (e, cur3) <- parseExpr rhsCtx cur2
        pure ((name, e), cur3)

    layoutIPBinds bindCol cur acc = do
        (item, cur') <- parseOneIPBind bindCol cur
        let acc' = item : acc
            (peek, curAfterPeek) = nextSig ctx cur'
        case tkKind peek of
            TkIn   -> pure (reverse acc', cur')
            TkEof  -> pure (reverse acc', cur')
            -- @let ?x = e ;@ with a stray explicit `;` separator before
            -- `in` (as in @let ?ctx = frozen; in body@) — eat the `;`
            -- and stop if @in@ follows immediately, otherwise continue
            -- (another `?ref` binding may follow).
            TkSemi ->
                let (after, _) = nextSig ctx curAfterPeek in
                case tkKind after of
                    TkIn  -> pure (reverse acc', curAfterPeek)
                    TkEof -> pure (reverse acc', curAfterPeek)
                    _     -> layoutIPBinds bindCol curAfterPeek acc'
            TkImplicitRef _ | tkCol peek == bindCol ->
                layoutIPBinds bindCol cur' acc'
            _ -> pure (reverse acc', cur')

    bracedIPBinds cur acc = do
        (item, cur3) <- parseOneIPBind 0 cur
        let (sep, curN) = nextSig ctx cur3
        case tkKind sep of
            TkSemi   -> bracedIPBinds curN (item : acc)
            TkRBrace -> pure (reverse (item : acc), curN)
            _        -> parseErr ctx "expected `;` or `}` in implicit let" sep

parseCase :: Ctx -> Cursor -> IO (Expr, Cursor)
parseCase ctx cur0 = do
    -- Full expression as scrutinee so @case x $ y of …@ and
    -- @case (e :: T) of …@ with bare @e :: T@ work.  parseExpr's
    -- trailing-sig pass consumes the optional @:: T@ as ETyApp
    -- metadata and stops at the @of@ keyword.
    (scrut, curS) <- parseExpr ctx cur0
    let (ofTok, curO) = nextSig ctx curS
    case tkKind ofTok of
        TkOf -> pure ()
        _    -> parseErr ctx "expected `of` in case-expression" ofTok
    let (firstTok, curBody) = nextSig ctx curO
    (alts, curEnd) <- case tkKind firstTok of
        TkLBrace -> bracedAlts ctx curBody []
        _        -> layoutAlts ctx (tkCol firstTok) curO []
    pure (buildCaseExpr scrut alts, curEnd)

-- | An alt body captured from parsing — either a plain @-> expr@ or a
-- list of guard clauses @| g1 -> e1 | g2 -> e2 …@ where each @g@ is a
-- comma-separated list of 'Guard' (bool or pattern @p <- expr@).  Both
-- forms may be wrapped in a where-clause that's already folded in.
data AltBody
    = AltBodyExpr !Expr
    | AltBodyGuards ![([Guard], Expr)]

-- | Assemble a case expression from its scrutinee and parsed alts,
-- desugaring any guarded alts so a failed guard falls through to the
-- next alt.  Fast path: if no alt has guards, emit a plain 'ECase'.
-- Guarded alts reuse the shared 'guardChain'/'guardStep' desugaring
-- already used by top-level function-clause guards.
buildCaseExpr :: Expr -> [(Pat, AltBody)] -> Expr
buildCaseExpr scrut alts
    | all (isPlain . snd) alts =
        ECase scrut [Alt pat body | (pat, AltBodyExpr body) <- alts]
    | otherwise =
        -- Let-bind the scrutinee once so the fallthrough cases don't
        -- recompute it, then right-fold the alts into nested ECases.
        let scrutName = BC.pack "$casescr"
            scrutE    = EVar scrutName
            failE     = EApp (EVar "error")
                          (stringToConsList "Non-exhaustive patterns in case")
            go (i, (pat, altBody)) tailE =
                let kName = BC.pack "$casek" <> BC.pack (show (i :: Int))
                    kVar  = EVar kName
                    body  = case altBody of
                        AltBodyExpr e       -> e
                        AltBodyGuards gs    -> guardChain gs kVar
                in ELet [(kName, tailE)]
                        (ECase scrutE
                            [ Alt pat body
                            , Alt PWild kVar
                            ])
        in ELet [(scrutName, scrut)]
                (foldr go failE (zip [0 ..] alts))
  where
    isPlain (AltBodyExpr _) = True
    isPlain _               = False

bracedAlts :: Ctx -> Cursor -> [(Pat, AltBody)] -> IO ([(Pat, AltBody)], Cursor)
bracedAlts ctx cur acc = do
    (alt, cur') <- parseAlt ctx ctx cur
    let (sep, curN) = nextSig ctx cur'
    case tkKind sep of
        TkSemi   -> bracedAlts ctx curN (alt : acc)
        TkRBrace -> pure (reverse (alt : acc), curN)
        _        -> parseErr ctx "expected `;` or `}` in case alts" sep

layoutAlts :: Ctx -> Int -> Cursor -> [(Pat, AltBody)] -> IO ([(Pat, AltBody)], Cursor)
layoutAlts ctx altCol cur acc = do
    let altCtx = ctx { ctxMinCol = altCol }
    (alt, cur') <- parseAlt ctx altCtx cur
    let (nextTok, curAfterSep) = nextSig ctx cur'
    case tkKind nextTok of
        TkEof -> pure (reverse (alt : acc), cur')
        -- Explicit @;@ separator between alts (Haskell 2010 §2.7):
        -- @case x of p1 -> e1; p2 -> e2@ is valid without braces.
        -- IHP's inline @\b -> case …readInt b of Just (n, "") -> …;
        -- _ -> Nothing@ form relies on this.
        TkSemi -> layoutAlts ctx altCol curAfterSep (alt : acc)
        -- A @where@ at the alt column does NOT start a new alternative
        -- (`where` is a reserved keyword, never a pattern).  It is the
        -- trailing where-clause of the *enclosing* equation/let/lambda,
        -- e.g. ghc-prim's
        --   x# `divModInt#` y# = case … of
        --     (# q#, r# #) -> (# … #)
        --     where !yn# = …
        -- where the single alt and the @where@ share column 3.  Stop
        -- the alts loop so the equation-level 'attachWhere' (or the
        -- lambda/let equivalent) consumes the binds.  Without this the
        -- loop calls 'parseAlt' on @where@ and dies with
        -- "expected pattern … saw TkWhere".
        TkWhere | tkCol nextTok == altCol ->
              pure (reverse (alt : acc), cur')
        _ | tkCol nextTok == altCol ->
              layoutAlts ctx altCol cur' (alt : acc)
          | otherwise ->
              pure (reverse (alt : acc), cur')

-- | Parse one case alternative.  Produces a @(Pat, AltBody)@ so the
-- caller (buildCaseExpr) can decide whether the alts need guard
-- fall-through desugaring.  Alt may be @pat -> expr@, @pat | g -> e [| g -> e …]@,
-- and may carry a trailing @where@ block (desugared to an ELet over the body).
parseAlt :: Ctx -> Ctx -> Cursor -> IO ((Pat, AltBody), Cursor)
parseAlt ctx altCtx cur = do
    (pat, cur1) <- parseTopPat ctx cur
    let (sepTok, cur2) = nextSig ctx cur1
    (body, cur3) <- case tkKind sepTok of
        TkArrow -> do
            (e, curE) <- parseExpr altCtx cur2
            pure (AltBodyExpr e, curE)
        TkBar -> do
            -- sepTok consumed the leading `|`; parseAltGuardBranches
            -- starts reading the guard list directly.
            (branches, curE) <- parseAltGuardBranches altCtx cur2 []
            pure (AltBodyGuards branches, curE)
        _    -> parseErr ctx "expected `->` in case alternative" sepTok
    -- Optional @where@ clause attached to this case alternative.
    -- Haskell allows @pat -> expr where { binds }@; we desugar to
    -- @pat -> let { binds } in expr@.  For guarded alts, we wrap every
    -- guard body with the same let so they all see the where-binds.
    -- The where must be indented strictly deeper than the alt
    -- pattern's column, otherwise it belongs to the enclosing binding.
    let (peekWhere, curAfterWhere) = nextSig altCtx cur3
    case tkKind peekWhere of
        TkWhere | tkCol peekWhere > ctxMinCol altCtx -> do
            (binds, curEnd) <- parseAltWhereBinds altCtx curAfterWhere
            case binds of
                [] -> pure ((pat, body), cur3)
                bs -> pure ((pat, wrapAltBodyLet bs body), curEnd)
        _ -> pure ((pat, body), cur3)
  where
    wrapAltBodyLet bs (AltBodyExpr e)         = AltBodyExpr (ELet bs e)
    wrapAltBodyLet bs (AltBodyGuards gs)      =
        AltBodyGuards [(g, ELet bs e) | (g, e) <- gs]

-- | Parse @| guard -> expr [| guard -> expr …]@ branches after a
-- case-alt pattern.  Each @guard@ is a comma-separated list of bool
-- guards or pattern guards (@p <- expr@); delegated to 'parseGuardList'.
parseAltGuardBranches :: Ctx -> Cursor -> [([Guard], Expr)] -> IO ([([Guard], Expr)], Cursor)
parseAltGuardBranches ctx cur acc = do
    (gs, cur1) <- parseGuardList ctx cur
    let (arr, cur2) = nextSig ctx cur1
    case tkKind arr of
        TkArrow -> pure ()
        _       -> parseErr ctx "expected `->` after case guard" arr
    (b, cur3) <- parseExpr ctx cur2
    let acc' = acc ++ [(gs, b)]
        (sep, cur4) = nextSig ctx cur3
    case tkKind sep of
        TkBar -> parseAltGuardBranches ctx cur4 acc'
        _     -> pure (acc', cur3)

-- | Parse the bindings of a @where@ clause attached to a case alternative.
-- Uses layout: all bindings at the same column as the first binding token,
-- and the block ends when we see a token at a column strictly less than
-- that column.
parseAltWhereBinds :: Ctx -> Cursor -> IO ([Bind], Cursor)
parseAltWhereBinds ctx cur0 = do
    let (firstTok, _) = nextSig ctx cur0
    case tkKind firstTok of
        TkEof -> pure ([], cur0)
        _     -> do
            let bindCol = tkCol firstTok
                innerCtx = ctx { ctxMinCol = bindCol }
            collect innerCtx bindCol cur0 []
  where
    collect innerCtx bindCol cur acc = do
        let (tok, cur1) = nextSig innerCtx cur
        case tkKind tok of
            TkEof -> pure (reverse acc, cur)
            _ | tkCol tok < bindCol && tkCol tok > 0 ->
                  pure (reverse acc, cur)
              | tkCol tok == bindCol -> do
                  (bs, cur') <- parseOneAltWhereBind innerCtx cur cur1 tok
                  collect innerCtx bindCol cur' (reverse bs ++ acc)
              | otherwise ->
                  -- Shouldn't happen if layout is sane; bail out.
                  pure (reverse acc, cur)

    -- Parse one @where@ binding: either @name [pats] = expr@ or
    -- @pat = expr@ (pattern binding).  Returns a list of (name, expr)
    -- because pattern bindings expand to multiple.
    -- @curBefore@ points to the first token of this binding (used for
    -- pattern bindings where parseSubPat needs to see the opening paren),
    -- @curAfter@ points past the first token (used when the first token
    -- is an identifier and we want to start collecting params/args).
    parseOneAltWhereBind innerCtx curBefore curAfter nameTok = case tkKind nameTok of
        TkIdent name
            | hasTopLevelConOpBeforeEq innerCtx curBefore -> do
                -- Unparenthesised infix conop pattern: `a :| as = ne`
                (pat, curPat) <- parseTopPat innerCtx curBefore
                let (sepTok, curSep) = nextSig innerCtx curPat
                case tkKind sepTok of
                    TkEq -> do
                        (rhsE, curE) <- parseExpr innerCtx curSep
                        let tmpName = BC.pack "$altwh0"
                            tmpBind = (tmpName, rhsE)
                            vars = patVarsAlt pat
                            perVarBind v =
                                ( v
                                , ECase (EVar tmpName)
                                    [ Alt (PIrref pat) (EVar v)
                                    , Alt PWild (EApp (EVar "error")
                                                  (stringToConsList
                                                    "Non-exhaustive pattern in alt-where"))
                                    ]
                                )
                        pure (tmpBind : map perVarBind vars, curE)
                    _ -> pure ([], curPat)
            | otherwise -> do
            -- Simple variable binding: collect params then parse body.
            (params, curP) <- collectLetParams innerCtx curAfter []
            let (sepTok, curSep) = nextSig innerCtx curP
            case tkKind sepTok of
                TkEq -> do
                    (expr, curE) <- parseExpr innerCtx curSep
                    let body = wrapParams params expr
                    pure ([(name, body)], curE)
                _ -> pure ([], curP)  -- give up silently
        TkLParen -> do
            -- Pattern binding: @(w, s'') = ...@
            (pat, curPat) <- parseSubPat innerCtx curBefore
            let (sepTok, curSep) = nextSig innerCtx curPat
            case tkKind sepTok of
                TkEq -> do
                    (rhsE, curE) <- parseExpr innerCtx curSep
                    let tmpName = BC.pack "$altwh0"
                        tmpBind = (tmpName, rhsE)
                        vars = patVarsAlt pat
                        perVarBind v =
                            ( v
                            , ECase (EVar tmpName)
                                [ Alt (PIrref pat) (EVar v)
                                , Alt PWild (EApp (EVar "error")
                                              (stringToConsList
                                                "Non-exhaustive pattern in alt-where"))
                                ]
                            )
                    pure (tmpBind : map perVarBind vars, curE)
                _ -> pure ([], curPat)
        _ -> pure ([], curBefore)

    patVarsAlt (PVar n)         = [n]
    patVarsAlt (PAs n p)        = n : patVarsAlt p
    patVarsAlt (PBang p)        = patVarsAlt p
    patVarsAlt (PIrref p)       = patVarsAlt p
    patVarsAlt (PTuple ps)      = concatMap patVarsAlt ps
    patVarsAlt (PCon _ ps)      = concatMap patVarsAlt ps
    patVarsAlt (PRecord _ fps)  = concatMap (patVarsAlt . snd) fps
    patVarsAlt (PRecordWild _)  = []
    patVarsAlt (PView _ p)      = patVarsAlt p
    patVarsAlt PWild            = []
    patVarsAlt (PLit _)         = []


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
            -- @:@ is the canonical list cons; keep the dedicated case.
            TkColon -> do
                (rhs, cur2) <- parseTopPat ctx cur1
                pure (PCon ":" [p, rhs], cur2)
            -- Haskell 2010 §4.1.2: any symbolic operator starting with
            -- @:@ is a constructor operator (e.g. @:|@ for NonEmpty,
            -- @:*:@ for Generics) and CAN appear as an infix pattern.
            -- Without this branch, @fmap f (x :| xs) = …@ failed with
            -- "expected `)` or `,` in pattern" the moment the parser
            -- walked past @x@ and saw @:|@.
            TkSymOp op | isConOp op -> do
                (rhs, cur2) <- parseTopPat ctx cur1
                pure (PCon op [p, rhs], cur2)
            _ -> pure (p, cur0)

    isConOp bs = case BC.uncons bs of
        Just (':', _) -> True
        _             -> False

parseTopPatNoCons :: Ctx -> Cursor -> IO (Pat, Cursor)
parseTopPatNoCons ctx cur = do
    let (tok, cur1) = nextSig ctx cur
    case tkKind tok of
        TkConId n  -> do
            (qname, cur2) <- readQualConId ctx n tok cur1
            collectArgs ctx (stripQualifier qname) [] cur2
        TkPrimId n
            | primIdStartsCon n ->
                collectArgs ctx n [] cur1
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
        -- @TkMinus@ is ambiguous as an unparenthesised constructor arg:
        --
        --   * @I# x - I# y = …@  (Num Int)         — binary infix @-@
        --     separating this ctor pattern from the next infix-LHS
        --     pattern.  Stop collecting; @-@ is the operator.
        --
        --   * @integerMul x (IS -1#) = …@  (ghc-bignum)  — @-1#@ is a
        --     NegativeLiterals sub-pattern argument of @IS@.  Parse it.
        --
        -- Disambiguate by the token after @-@: a numeric literal
        -- (@TkInt@ / @TkFloat@) means a negative-literal arg (delegate
        -- to 'parseSubPat', which already handles @-1#@ / @-1@ /
        -- @-1.0@); anything else (ident, ConId, paren, …) is the infix
        -- @-@ operator, so we stop as before.
        TkMinus ->
            let (afterMinus, _) = nextSig ctx cur' in
            case tkKind afterMinus of
                TkInt _   -> do
                    (sp, cur'') <- parseSubPat ctx cur
                    collectArgs ctx name (sp : acc) cur''
                TkFloat _ -> do
                    (sp, cur'') <- parseSubPat ctx cur
                    collectArgs ctx name (sp : acc) cur''
                _ -> pure (PCon name (reverse acc), cur)
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
        -- Lazy/irrefutable pattern: ~pat (Haskell Report §3.17.3).
        -- The match always succeeds; bound variables become thunks that
        -- re-attempt the match on force. Eval's matchPat handles PIrref;
        -- here we just preserve the syntactic distinction.
        TkSymOp op | op == BC.pack "~" -> do
            (p, curN) <- parseSubPat ctx cur1
            pure (PIrref p, curN)
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
        TkPrimId n
            | primIdStartsCon n -> pure (PCon n [], cur1)
            | otherwise         -> pure (PVar n, cur1)   -- e.g. state# param in primop binding
        TkUnderscore -> pure (PWild, cur1)
        -- Mirror the expression-side routing at parseAtom (see
        -- IHC.Parser § "TkInt n | n <= maxBound :: Int64") — keep
        -- arbitrary-precision values as 'LInteger' instead of
        -- silently truncating via 'fromInteger' into a wrapped Int64.
        -- Otherwise @case x of 9223372036854775808 -> ...@ would
        -- match against @-9223372036854775808@ — a soundness bug.
        TkInt n
            | n <= toInteger (maxBound :: Int64)
                -> pure (PLit (LInt (fromInteger n)), cur1)
            | otherwise
                -> pure (PLit (LInteger n), cur1)
        TkFloat d    -> pure (PLit (LFloat d), cur1)
        TkStr s      -> pure (PLit (LStr s), cur1)
        TkChar c     -> pure (PLit (LChar c), cur1)
        TkConId n    -> do
            -- Handle qualified constructor: M.Ctor or M.N.Ctor.
            -- In atomic-pattern positions (lambda params, function lhs
            -- params, list elems, etc.) a constructor can still carry
            -- record syntax, e.g. `User{..}` / `User{name}`. Those forms
            -- must parse to PRecord/PRecordWild here so the scheduler can
            -- later desugar them to positional PCon patterns.
            (qname, cur2) <- readQualConId ctx n tok cur1
            let name = stripQualifier qname
                (nextTok, cur3) = nextSig ctx cur2
            case tkKind nextTok of
                TkLBrace -> do
                    (fieldPats, curEnd, isWild) <- parseRecordPatFields ctx name cur3 []
                    if isWild
                        then pure (PRecordWild name, curEnd)
                        else pure (PRecord name fieldPats, curEnd)
                _ ->
                    pure (PCon name [], cur2)
        TkLParen     -> parseParenPat ctx cur1
        TkLBracket   -> parseListPat ctx cur1
        TkLUnbox     -> parseUnboxedTuplePat ctx cur1
        TkMinus -> do
            let (n, cur2) = nextSig ctx cur1
            case tkKind n of
                -- Same routing as the unsigned arm above, applied to
                -- the negated value: stay in 'LInt' when the Int64
                -- range can hold it (note that @minBound :: Int64@ is
                -- one slot below @-(maxBound :: Int64)@, so the
                -- comparison must be against @minBound@ post-negate).
                TkInt i ->
                    let neg = negate i in
                    if neg >= toInteger (minBound :: Int64)
                        then pure (PLit (LInt (fromInteger neg)), cur2)
                        else pure (PLit (LInteger neg), cur2)
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
            TkDColon | depth == 0 -> False   -- typed pattern; arrows belong to the type
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
            -- Use parseTopPatNoCons so constructor-applied list elements
            -- like @[PG.Only result]@ parse as @PCon "PG.Only" [result]@
            -- instead of stopping at the bare @PG.Only@.  We skip the
            -- infix `:` tail that 'parseTopPat' would append — list
            -- literals don't nest `:` between commas.
            (first, cur2) <- parseTopPatNoCons ctx cur
            gatherListPat ctx [first] cur2

gatherListPat :: Ctx -> [Pat] -> Cursor -> IO (Pat, Cursor)
gatherListPat ctx acc cur = do
    let (tok, cur1) = nextSig ctx cur
    case tkKind tok of
        TkComma -> do
            (p, cur2) <- parseTopPatNoCons ctx cur1
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
parseUnboxedTuplePat ctx cur0
    -- @(# #)@ — nullary unboxed tuple.
    | isUnboxClose ctx cur0 =
        pure (PCon "(##)" [], skipUnboxClose ctx cur0)
    -- @(# | ... #)@ — unboxed sum with an empty first slot
    -- (the value lives in alternative ≥ 2).
    | (t0, _) <- nextSig ctx cur0
    , tkKind t0 == TkBar =
        parseUnboxedSumPat ctx 1 Nothing cur0
    | otherwise = do
        (first, cur1) <- parseTopPat ctx cur0
        -- A @|@ here means this is an unboxed sum, not a tuple:
        -- @first@ is alternative 1's payload.
        let (sep, _) = nextSig ctx cur1
        case tkKind sep of
            TkBar -> parseUnboxedSumPat ctx 1 (Just (1, first)) cur1
            _     -> gatherUnboxedTuplePat ctx [first] cur1

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

-- | Parse the tail of an unboxed-sum pattern, starting at the
-- separator/close after slot @curIdx@.  @mFound@ carries the
-- payload pattern + its 1-based slot index once seen (exactly one
-- slot is non-empty in a well-formed sum pattern).  Desugars to
-- @PCon "(#|#)" [PLit (LInt tag), payloadPat]@ so matchPat's
-- generic VCon zip checks the tag and binds the payload.
--
-- Grammar handled (2-alt is all ghc-bignum uses, but N-alt works):
--   @(# p  |    #)@   slot 1 = p,   slot 2 empty   → tag 1
--   @(#    | p  #)@   slot 1 empty, slot 2 = p     → tag 2
--   @(# (# #) | #)@   slot 1 = @(##)@, slot 2 empty → tag 1
parseUnboxedSumPat
    :: Ctx -> Int -> Maybe (Int, Pat) -> Cursor -> IO (Pat, Cursor)
parseUnboxedSumPat ctx curIdx mFound cur
    | isUnboxClose ctx cur =
        case mFound of
            Just (idx, p) ->
                pure ( PCon (BC.pack "(#|#)")
                            [PLit (LInt (fromIntegral idx)), p]
                     , skipUnboxClose ctx cur )
            Nothing ->
                parseErr ctx
                    "unboxed sum pattern has no payload slot"
                    (fst (nextSig ctx cur))
    | otherwise = do
        let (tok, cur1) = nextSig ctx cur
        case tkKind tok of
            -- Separator: advance to the next alternative slot.
            TkBar -> parseUnboxedSumPat ctx (curIdx + 1) mFound cur1
            -- A pattern at the current slot.  Record it (its slot
            -- index is curIdx) and continue scanning for the close.
            _ -> do
                (p, cur2) <- parseTopPat ctx cur
                case mFound of
                    Just _  -> parseErr ctx
                                 "unboxed sum pattern has more than one payload"
                                 tok
                    Nothing -> parseUnboxedSumPat ctx curIdx
                                 (Just (curIdx, p)) cur2

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
        _ | Just (fname, curName) <- readRecordFieldName ctx tok cur' -> do
            let (eqTok, cur2) = nextSig ctx curName
            case tkKind eqTok of
                TkEq -> do
                    (p, cur3) <- parseTopPat ctx cur2
                    parseRecordPatFields ctx _conName cur3 ((fname, p) : acc)
                -- NamedFieldPuns: { x } → { x = x }
                -- Note: use cur' (not cur2) so we don't consume the non-`=` token.
                _ -> parseRecordPatFields ctx _conName curName ((fname, PVar fname) : acc)
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

primIdStartsCon :: ByteString -> Bool
primIdStartsCon bs =
    case BC.uncons bs of
        Just (c, _) -> c >= 'A' && c <= 'Z'
        Nothing     -> False

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
-- The @ctxMinCol@ check: operators strictly BEFORE the enclosing layout
-- column belong to the outer construct.  Operators at or deeper than
-- the layout column continue the current expression — in particular a
-- @|>@ pipeline formatted as
--
-- > do
-- >   x
-- >   |> f
-- >   |> g
--
-- starts each @|>@ at the statement column, and Haskell treats those as
-- continuation of the preceding expression (even though @|>@ sits at
-- the same column as a fresh statement would).
--
-- Exception: tokens that ALSO start patterns (@!@, @~@) at exactly the
-- layout column are sibling pattern-bindings, not operator continuations.
-- E.g. in a where-block:
--
-- > foo = bar
-- >   where
-- >     !x = 10
-- >     !y = 20
--
-- the @!@ of @!y@ sits at the bind column and must terminate the @10@
-- expression rather than be consumed as @10 ! y@.
peekOp :: Ctx -> Cursor -> Maybe (Name, Assoc, Int, Cursor)
peekOp ctx cur =
    let (tok, cur') = nextSig ctx cur
        col         = tkCol tok
        minCol      = ctxMinCol ctx
        beforeLayout = minCol > 0 && col < minCol
        atLayoutAndPatternStart =
            minCol > 0 && col == minCol && isPatternStartOp (tkKind tok)
    in
    if beforeLayout || atLayoutAndPatternStart
        then Nothing
        else case tkKind tok of
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
--
-- 'TkPrimId' is accepted alongside 'TkIdent' / 'TkConId' so that
-- MagicHash primops can be used in backticked-operator position, e.g.
-- @x \`eqChar#\` y@ in @ghc-prim-0.12.0/GHC/Classes.hs:301@:
--
-- > (C# x) \`eqChar\` (C# y) = isTrue# (x \`eqChar#\` y)
--
-- Without this, the parser bails on the inner backtick, the body parse
-- of @eqChar@ throws 'ParseError', and 'discoverInModuleWith'' silently
-- records @eqChar@ as a discovery miss — which then surfaces as
-- @IHC.Eval: unbound variable \`eqChar\`@ when the source-loaded
-- @instance Eq Char where (==) = eqChar@ method body is forced.
readBacktickName :: Ctx -> Cursor -> Maybe (Name, Cursor)
readBacktickName ctx = goFirst
  where
    goFirst cur =
        let (tok, cur1) = nextSig ctx cur
        in case tkKind tok of
            TkIdent  n -> goRest n cur1
            TkConId  n -> goRest n cur1
            TkPrimId n -> goRest n cur1
            _          -> Nothing

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
    TkBang     -> Just "!"
    TkDollar   -> Just "$"
    TkSymOp n  -> Just n
    _          -> Nothing

-- | True iff this token can start a pattern (and therefore a sibling
-- pattern-binding when it appears at the layout column). Used by
-- 'peekOp' to refuse to swallow an @!x@ / @~y@ at exactly the bind
-- column as an infix operator continuation.
isPatternStartOp :: TokenKind -> Bool
isPatternStartOp TkBang        = True
isPatternStartOp (TkSymOp op)  = op == BC.pack "~"
isPatternStartOp _             = False

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
    (head_, cur1) <- parseAtomPostfix ctx cur0
    loop head_ cur1
  where
    loop fn cur =
        let (tok, cur') = nextSig ctx cur in
        case tkKind tok of
            -- TypeApplications: @T is a value-level type hint. We capture
            -- the raw source bytes of the type argument into an 'ETyApp'
            -- node so downstream passes (e.g. Typeable / dictionary
            -- selection) can read it, but the evaluator itself treats
            -- 'ETyApp' as a pass-through on the inner expression.
            -- But @?=, @=?, etc. are operator-infix uses, not type applications:
            -- peek at the token right after '@' (without skipping whitespace).
            -- If it's a TkSymOp, leave for the Pratt operator parser.
            TkAt ->
                let (nextTok, _) = nextToken (ctxSrc ctx) cur'
                in case tkKind nextTok of
                    TkSymOp _ -> pure (fn, cur)   -- operator, hand off to Pratt
                    _ -> do
                        (tyArg, cur'') <- captureTypeArg ctx cur'
                        loop (ETyApp fn tyArg) cur''
            -- Record-update: expr { field = val, ... }
            -- Disambiguate from block syntax: only treat as record-update when
            -- we see '{' immediately followed by 'ident =' or '}'.
            TkLBrace | isRecordUpdateBrace ctx cur' -> do
                    (fields, curEnd) <- parseRecordUpdateFields ctx cur' []
                    loop (ERecordUpdate fn fields) curEnd
            -- BlockArguments (IHP default-extension): @do@, @case@,
            -- @let … in …@, @if … then … else …@, and lambdas can
            -- serve as function arguments without parens.  We consume
            -- one of these block-forms as a single atom argument.
            -- Column gate still applies so a `do` on a less-indented
            -- line doesn't get stolen from the enclosing construct.
            _ | isBlockArgStart (tkKind tok) && tkCol tok > ctxMinCol ctx -> do
                    (arg, curA) <- parseBlockArg ctx cur
                    loop (EApp fn arg) curA
            _ | startsAtom (tkKind tok) && tkCol tok > ctxMinCol ctx -> do
                    (arg, curA) <- parseAtomPostfix ctx cur
                    loop (EApp fn arg) curA
            -- NegativeLiterals (always-on): @TkMinus@ /immediately/
            -- followed by a numeric literal in argument position is a
            -- single negative literal, not binary subtraction.  The
            -- adjacency check (no whitespace between '-' and the
            -- digit) preserves the standard Haskell behaviour for
            -- @f - 1@ (subtraction) — only @f -1@ becomes @f (-1)@.
            -- Required by ghc-bignum's source, which uses this form
            -- pervasively (e.g. @intToInt64# INT_MINBOUND#@ where
            -- @INT_MINBOUND@ expands to @-0x8000000000000000@).
            TkMinus
              | tkCol tok > ctxMinCol ctx
              , tkStart tok > cPos cur
              , let (litTok, _) = nextToken (ctxSrc ctx) cur'
              , tkStart litTok == tkEnd tok
              , case tkKind litTok of
                  TkInt _   -> True
                  TkFloat _ -> True
                  _         -> False
              -> do
                    (arg, curA) <- parseAtomPostfix ctx cur'
                    loop (EApp fn (ENeg arg)) curA
              | otherwise -> pure (fn, cur)
            _ | otherwise -> pure (fn, cur)

    isBlockArgStart k = case k of
        TkDo        -> True
        TkCase      -> True
        TkIf        -> True
        TkLet       -> True
        TkBackslash -> True
        _           -> False

    parseBlockArg c cur = do
        let (tok, cur1) = nextSig c cur
        case tkKind tok of
            TkDo        -> parseDo     c cur1
            TkCase      -> parseCase   c cur1
            TkIf        -> parseIf     c cur1
            TkLet       -> parseLet    c cur1
            TkBackslash -> parseLambda c cur1
            _           -> parseAtomPostfix c cur

-- | Parse an atom plus postfix forms that bind tighter than function
-- application. In particular, @f x{a=b}@ must parse as @f (x{a=b})@,
-- not @(f x){a=b}@.
parseAtomPostfix :: Ctx -> Cursor -> IO (Expr, Cursor)
parseAtomPostfix ctx cur0 = do
    (atom, cur1) <- parseAtom ctx cur0
    loop atom cur1
  where
    loop expr cur =
        let (tok, cur') = nextSig ctx cur in
        case tkKind tok of
            TkLBrace | isRecordUpdateBrace ctx cur' -> do
                (fields, curEnd) <- parseRecordUpdateFields ctx cur' []
                loop (ERecordUpdate expr fields) curEnd
            _ -> pure (expr, cur)

-- | Peek ahead after @{@ to decide whether this is a record-update
-- expression @expr { f = e, ... }@. Returns 'True' when the tokens
-- immediately after @{@ are @ident =@, @ident }@, @ident ,@ (field-update
-- or NamedFieldPun syntax), or @}@ (empty update).
-- Returns 'False' otherwise (e.g. a do-block @{ stmt; }@).
isRecordUpdateBrace :: Ctx -> Cursor -> Bool
isRecordUpdateBrace ctx cur =
    let (t1, cur1) = nextSig ctx cur in
    case readRecordFieldName ctx t1 cur1 of
        Just (_, curName) ->
            let (t2, _) = nextSig ctx curName in
            case tkKind t2 of
                TkEq     -> True   -- { field = ...
                TkRBrace -> True   -- { field }  (NamedFieldPun)
                TkComma  -> True   -- { field, ... } (NamedFieldPun list)
                _        -> False
        Nothing ->
            case tkKind t1 of
                TkRBrace -> True          -- empty update { }
                _        -> False

-- | Parse an unqualified or module-qualified record field name, returning the
-- selector name itself.  Source such as @NS.defaultHints { NS.addrFlags = x }@
-- qualifies fields with the imported module alias, but the field registry and
-- desugaring code key selectors by their unqualified name.
readRecordFieldName :: Ctx -> Token -> Cursor -> Maybe (Name, Cursor)
readRecordFieldName ctx tok0 cur0 = do
    (first, cur1) <- segment (tkKind tok0) cur0
    go first cur1
  where
    segment kind cur = case kind of
        TkIdent n -> Just (n, cur)
        TkConId n -> Just (n, cur)
        _         -> Nothing

    go lastName cur =
        let (dotTok, curDot) = nextSig ctx cur in
        case tkKind dotTok of
            TkDot ->
                let (segTok, curSeg) = nextSig ctx curDot in
                case segment (tkKind segTok) curSeg of
                    Just (n, curNext) -> go n curNext
                    Nothing           -> Nothing
            _ -> Just (lastName, cur)

-- | Parse a record-update field list @{ f1 = e1, f2 = e2, ... }@.
-- The opening @{@ has NOT been consumed; @cur@ is positioned just after @{@.
-- Supports NamedFieldPuns: @{ x }@ → @{ x = x }@.
parseRecordUpdateFields :: Ctx -> Cursor -> [(Name, Expr)] -> IO ([(Name, Expr)], Cursor)
parseRecordUpdateFields ctx cur acc = do
    let (tok, cur') = nextSig ctx cur
    case tkKind tok of
        TkRBrace -> pure (reverse acc, cur')
        TkComma  -> parseRecordUpdateFields ctx cur' acc
        _ | Just (fname, curName) <- readRecordFieldName ctx tok cur' -> do
            let (eqTok, cur2) = nextSig ctx curName
            case tkKind eqTok of
                TkEq -> do
                    (e, cur3) <- parseExpr ctx cur2
                    parseRecordUpdateFields ctx cur3 ((fname, e) : acc)
                -- NamedFieldPun: { x } → { x = x }
                -- Don't consume the non-`=` token (use cur', not cur2).
                _ -> parseRecordUpdateFields ctx curName ((fname, EVar fname) : acc)
        TkEof -> pure (reverse acc, cur')
        _ -> parseErr ctx "expected field name or `}` in record update" tok

-- | Skip one type-argument after @\@@ in a type application, returning
-- the cursor advanced past the consumed argument and the raw source-byte
-- slice that spans it (opening bracket/tick through closing token).
-- Handles: plain ident/conid (@\@Int@, @\@a@), promoted (@\@\'Foo@),
-- and balanced paren/bracket groups (@\@(Maybe Int)@, @\@[Int]@).
-- Used to produce 'ETyApp' nodes that retain the type for later
-- inspection. Returns an empty string if nothing was consumed.
captureTypeArg :: Ctx -> Cursor -> IO (Name, Cursor)
captureTypeArg ctx cur0 =
    let (tok, cur1) = nextSig ctx cur0
        start       = tkStart tok
        slice end   = sliceBytes (ctxSrc ctx) (start, end)
    in case tkKind tok of
        TkAt       -> pure (BC.empty, cur0)   -- bare '@' shouldn't nest; stop
        TkLParen   -> do
            cur2 <- skipBalanced ctx cur1 TkRParen
            pure (slice (cPos cur2), cur2)
        TkLBracket -> do
            cur2 <- skipBalanced ctx cur1 TkRBracket
            pure (slice (cPos cur2), cur2)
        -- Promoted-list/tuple tick: the lexer emits TkTick for `'[`/`'(`,
        -- then the structural token separately. Capture the whole
        -- `'[...]` / `'(...)` region.
        TkTick ->
            let (tok2, cur2) = nextSig ctx cur1 in
            case tkKind tok2 of
                TkLBracket -> do
                    cur3 <- skipBalanced ctx cur2 TkRBracket
                    pure (slice (cPos cur3), cur3)
                TkLParen -> do
                    cur3 <- skipBalanced ctx cur2 TkRParen
                    pure (slice (cPos cur3), cur3)
                -- `'True`/`'Nothing` — legacy shape now reached via TkTick
                -- followed by an identifier.
                TkConId{} -> pure (slice (tkEnd tok2), cur2)
                TkIdent{} -> pure (slice (tkEnd tok2), cur2)
                _         -> pure (slice (tkEnd tok), cur1)
        -- Legacy fallback: older lexings where `'X` was still TkChar 'X'.
        -- Kept for safety while the lexer migration settles.
        TkChar _   ->
            let (tok2, cur2) = nextSig ctx cur1 in
            case tkKind tok2 of
                TkIdent{} -> pure (slice (tkEnd tok2), cur2)
                TkConId{} -> pure (slice (tkEnd tok2), cur2)
                _         -> pure (slice (tkEnd tok), cur1)
        _ -> pure (slice (tkEnd tok), cur1)
            -- single token consumed (ident, conid, primid, strlit symbol, etc.)

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
startsAtom TkAddrStr{}     = True   -- Phase 2.x: "..."# Addr# literal
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
startsAtom TkQQOpen{}      = True  -- [hsx|…|] etc. — QuasiQuoter is a valid argument
startsAtom _               = False

--------------------------------------------------------------------------------
-- Atoms
--------------------------------------------------------------------------------

parseAtom :: Ctx -> Cursor -> IO (Expr, Cursor)
parseAtom ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        -- A.3: keep arbitrary-precision Integer literals out of Int64
        -- truncation. In-range numbers stay as 'LInt' (Int64) so the
        -- existing arithmetic primops fire normally; out-of-range
        -- literals produce 'LInteger' which evaluates to 'VInteger'.
        TkInt n
            | n >= toInteger (minBound :: Int64) &&
              n <= toInteger (maxBound :: Int64)
                  -> pure (ELit (LInt (fromInteger n)), cur1)
            | otherwise -> pure (ELit (LInteger n), cur1)
        TkFloat d  -> pure (ELit (LFloat d), cur1)
        TkStr s    -> pure (stringToConsList (BC.unpack s), cur1)
        -- @\"...\"#@ — unboxed string literal (Addr#).  Evaluator
        -- produces a 'VPrimObj (PrimPtr ptr)' pointing at a leaked
        -- malloc-backed copy of the bytes; bytestring's
        -- 'unsafePackLenLiteral' / 'allBytes' rely on this shape.
        TkAddrStr s -> pure (ELit (LAddrStr s), cur1)
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
                        TkLBrace | recordConstructorName qname -> do
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
        -- [name| ... |] — QuasiQuoter.  Skip the body as opaque bytes
        -- (tracking brace/paren/bracket depth so nested @[foo|...|]@ work)
        -- up to the closing @|]@ and emit an 'EQuasiQuote' node carrying
        -- the captured body bytes.  The evaluator resolves @qqName@ to a
        -- real 'QuasiQuoter' value and feeds the body through
        -- @quoteExp :: String -> Q Exp@ at run time.
        TkQQOpen qqName -> do
            curEnd <- skipQQBody ctx cur1
            let body = sliceBytes (ctxSrc ctx) (cPos cur1, cPos curEnd - 2)
            pure (EQuasiQuote qqName body, curEnd)
        TkEof -> parseErr ctx "unexpected end of input" tok
        _ -> parseErr ctx "unexpected token" tok
  where
    recordConstructorName qname =
        case reverse (BC.split '.' qname) of
            seg:_ | not (BC.null seg) ->
                let h = BC.head seg in h >= 'A' && h <= 'Z'
            _ -> False

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
                    -- Uses the synthetic $fldProj$ key so it works under
                    -- NoFieldSelectors.
                    let (closeTok, curClose) = nextSig ctx curAfterField
                    case tkKind closeTok of
                        TkRParen ->
                            let n = "$s"
                            in pure (ELam n (EApp (EVar (recordDotProjName fname)) (EVar n)), curClose)
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
            --   - `::` → type annotation (preserved as 'ETyApp' metadata
            --            when the annotation is the whole @( e :: T )@
            --            form; stripped silently when it precedes a
            --            tuple comma, e.g. @( 1 :: Int, 2 :: Int )@).
            --   - an operator + `)` → left section.
            --
            -- We use 'parseExprNoSig' so the @::@ token survives for the
            -- 'TkDColon' branch below to capture the type bytes. (The
            -- outer 'parseExpr' also swallows trailing signatures; inside
            -- parens we need to do that work ourselves.)
            (e, cur1) <- parseExprNoSig ctx cur0
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
                    -- @(e :: T)@ — type annotation. Two shapes:
                    --
                    --   * @(e :: T)@         — preserve @T@ as ETyApp metadata.
                    --   * @(e :: T, ...)@    — annotation on a tuple
                    --                          element; drop the type
                    --                          and continue collecting
                    --                          tuple elements.
                    (tyBytes, afterTy, stopTok) <- captureTypeUntilTupleOrClose ctx cur2
                    case stopTok of
                        TupleStopComma -> do
                            (rest, curEnd) <- gatherTupleSectionElems ctx afterTy []
                            let elems = Just e : rest
                            if any isTsHole elems
                                then pure (desugarTupleSection elems, curEnd)
                                else pure (ETuple (map fromJustTs elems), curEnd)
                        TupleStopClose | BS.null tyBytes -> pure (e, afterTy)
                        TupleStopClose                   -> pure (ETyApp e tyBytes, afterTy)
                        TupleStopEof                     -> pure (e, afterTy)
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
        _ | Just (fname, curName) <- readRecordFieldName ctx tok cur' -> do
            let (eqTok, cur2) = nextSig ctx curName
            case tkKind eqTok of
                TkEq -> do
                    (e, cur3) <- parseRecordFieldExpr ctx fname cur2
                    parseRecordFields ctx conName cur3 ((fname, e) : acc)
                -- NamedFieldPuns: { x } or { x, y } — no `=`, bind to same name.
                -- Note: use cur' (not cur2) so we don't consume the non-`=` token.
                _ -> parseRecordFields ctx conName curName ((fname, EVar fname) : acc)
        TkEof -> pure (reverse acc, cur', False)
        _ -> parseErr ctx "expected field name or `}` in record literal" tok

parseRecordFieldExpr :: Ctx -> Name -> Cursor -> IO (Expr, Cursor)
parseRecordFieldExpr ctx fname cur
    | fname == BC.pack "settingsHost" =
        let (tok, cur1) = nextSig ctx cur in
        case tkKind tok of
            TkStr s ->
                pure (hostPreferenceStringExpr (BC.unpack s), cur1)
            _ -> parseExpr ctx cur
    | otherwise = parseExpr ctx cur

hostPreferenceStringExpr :: String -> Expr
hostPreferenceStringExpr s =
    case s of
        "*"  -> EVar (hpCtor "HostAny")
        "*4" -> EVar (hpCtor "HostIPv4")
        "!4" -> EVar (hpCtor "HostIPv4Only")
        "*6" -> EVar (hpCtor "HostIPv6")
        "!6" -> EVar (hpCtor "HostIPv6Only")
        _    -> EApp (EVar (hpCtor "Host")) (stringToConsList s)
  where
    hpCtor n = BC.pack "Data.Streaming.Network.Internal." <> BC.pack n

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
        else
          -- @(# | ... #)@ — unboxed sum, empty first slot.
          let (t0, _) = nextSig ctx cur0 in
          if tkKind t0 == TkBar
            then parseUnboxedSumExpr ctx 1 Nothing cur0
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
                        -- @(# e | ... #)@ — unboxed sum, @e@ is
                        -- alternative 1's payload.
                        TkBar -> parseUnboxedSumExpr ctx 1 (Just (1, e)) cur1
                        _ -> parseErr ctx "expected `,`, `|` or `#)` in unboxed tuple/sum" sep
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

    -- Mirror 'parseUnboxedSumPat' for expression position.  Builds
    -- @(#|#) <tag> <payload>@ where tag is the 1-based index of the
    -- non-empty alternative slot.
    parseUnboxedSumExpr
        :: Ctx -> Int -> Maybe (Int, Expr) -> Cursor -> IO (Expr, Cursor)
    parseUnboxedSumExpr curCtx curIdx mFound cur
        | isUnboxClose curCtx cur =
            case mFound of
                Just (idx, e) ->
                    let con = EVar (BC.pack "(#|#)")
                    in pure ( EApp (EApp con (ELit (LInt (fromIntegral idx)))) e
                            , skipUnboxClose curCtx cur )
                Nothing ->
                    parseErr curCtx
                        "unboxed sum has no payload slot"
                        (fst (nextSig curCtx cur))
        | otherwise =
            let (tok, cur1) = nextSig curCtx cur in
            case tkKind tok of
                TkBar -> parseUnboxedSumExpr curCtx (curIdx + 1) mFound cur1
                _ -> do
                    (e, cur2) <- parseExpr curCtx cur
                    case mFound of
                        Just _  -> parseErr curCtx
                                     "unboxed sum has more than one payload" tok
                        Nothing -> parseUnboxedSumExpr curCtx curIdx
                                     (Just (curIdx, e)) cur2

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

-- | Where the parser stopped consuming type-annotation tokens.
--
--   * 'TupleStopClose' — saw the closing @)@ of the paren expression.
--   * 'TupleStopComma' — saw a top-level @,@ (the surrounding parens
--     are actually a tuple; the annotation applies to this tuple
--     element only).
--   * 'TupleStopEof'   — ran off the end of the input.
data TupleStop = TupleStopClose | TupleStopComma | TupleStopEof

-- | After consuming @::@ inside a parenthesised expression, capture
-- tokens at paren-depth 0 until we hit the enclosing @)@ or a @,@
-- (tuple element separator). Arbitrary type-level tokens are tolerated
-- (forall, =>, #, unboxed tuples, brackets).
--
-- Returns the (trimmed) raw bytes spanning the annotation, the cursor
-- positioned after the stop token (@)@ is consumed; @,@ is consumed),
-- and which kind of stop token we hit. The bytes preserve DataKinds
-- type literals on 'ETyApp' so runtime @symbolVal@ / @natVal@ dispatch
-- can recover the lifted value.
captureTypeUntilTupleOrClose :: Ctx -> Cursor -> IO (ByteString, Cursor, TupleStop)
captureTypeUntilTupleOrClose ctx cur0 = go cur0 (0 :: Int)
  where
    startPos = cPos cur0
    src      = ctxSrc ctx
    trim bs  = BC.dropWhile isSpace
                 (BC.reverse (BC.dropWhile isSpace (BC.reverse bs)))
    go cur depth = do
        let (tok, cur') = nextSig ctx cur
        case tkKind tok of
            TkEof                 ->
                pure (trim (sliceBytes src (startPos, cPos cur)), cur', TupleStopEof)
            TkRParen | depth == 0 ->
                pure (trim (sliceBytes src (startPos, tkStart tok)), cur', TupleStopClose)
            TkComma  | depth == 0 ->
                pure (trim (sliceBytes src (startPos, tkStart tok)), cur', TupleStopComma)
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
            TkDotDot | tkStart dotTok == tkEnd lastTok ->
                pure (EVar (acc <> BC.pack ".."), curAfterDot)
            TkDot | tkStart dotTok == tkEnd lastTok ->
                let (segTok, curAfterSeg) = nextToken src curAfterDot in
                case tkKind segTok of
                    TkIdent n | tkStart segTok == tkEnd dotTok ->
                        pure (EVar (acc <> BC.pack "." <> n), curAfterSeg)
                    TkConId n | tkStart segTok == tkEnd dotTok ->
                        go (acc <> BC.pack "." <> n) segTok curAfterSeg
                    _ | Just opName <- tokenOpName (tkKind segTok)
                      , tkStart segTok == tkEnd dotTok ->
                        pure (EVar (acc <> BC.pack "." <> opName), curAfterSeg)
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
                    -- Desugar: expr.fname -> ($fldProj$fname) expr, then recurse for chaining.
                    -- Using the synthetic $fldProj$ prefix keeps record-dot working
                    -- even under {-# LANGUAGE NoFieldSelectors #-}, where the bare-
                    -- name accessor is suppressed.  The scheduler binds both names:
                    -- bare for non-NoFieldSelectors modules, $fldProj$ for every
                    -- field globally.  See IHC.Scheduler.fieldProjName.
                    applyRecordDots ctx fieldTok
                        (EApp (EVar (recordDotProjName fname)) expr) curAfterField
                _ -> pure (expr, cur)
        _ -> pure (expr, cur)

-- | Build the synthetic env-key used by the record-dot desugaring.
-- Must match 'IHC.Scheduler.fieldProjName'.  The @$fldProj$@ prefix is
-- not a valid Haskell identifier, so it can never collide with a
-- user-defined name.
recordDotProjName :: ByteString -> ByteString
recordDotProjName fname = BC.pack "$fldProj$" <> fname

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
                        -- [first ..] — open-ended range = enumFrom first.
                        pure (EApp (EVar "enumFrom") first, snd (nextSig ctx cur1))
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
                                -- [first, second ..] — open-ended stepped range = enumFromThen.
                                pure ( EApp (EApp (EVar "enumFromThen") first) second
                                     , snd (nextSig ctx cur3) )
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
            -- List comprehension @[ e | q1, q2, ... ]@.  Parses each
            -- qualifier (@pat <- src@ generator or a boolean guard) and
            -- builds a nested desugared expression per the Haskell
            -- report:
            --   [e | p <- xs]     = concatMap (\$cv -> case $cv of { p -> [e]; _ -> [] }) xs
            --   [e | cond]        = if cond then [e] else []
            --   [e | q, qs]       = [[e | qs] | q]    (sequencing)
            TkBar -> parseListComp ctx first cur1
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

-- | Parse a list comprehension body: one or more qualifiers separated by
-- commas, followed by a closing @]@.  Uses the standard desugaring:
--   [e | p <- xs]     ~> concatMap (\$cv -> case $cv of { p -> [e]; _ -> [] }) xs
--   [e | cond]        ~> if cond then [e] else []
--   [e | q1, q2]      ~> process q1 wrapping @[e | q2]@ as the body.
--
-- The @headExpr@ is the element expression before the @|@.
parseListComp :: Ctx -> Expr -> Cursor -> IO (Expr, Cursor)
parseListComp ctx headExpr cur0 = do
    (quals, cur1) <- collectQuals [] cur0
    let resultExpr = foldr qualWrap (buildSingleton headExpr) quals
    pure (resultExpr, cur1)
  where
    collectQuals acc cur = do
        (q, cur1) <- parseQual ctx cur
        let (sep, curNext) = nextSig ctx cur1
        case tkKind sep of
            TkComma    -> collectQuals (q : acc) curNext
            TkRBracket -> pure (reverse (q : acc), curNext)
            _          -> parseErr ctx "expected `,` or `]` in list comprehension" sep

    buildSingleton e = EApp (EApp (EVar (BC.pack ":")) e) (EVar (BC.pack "[]"))

    qualWrap :: Qual -> Expr -> Expr
    qualWrap (QGen pat src) body =
        let var = BC.pack "$lcv"
            altMatch = Alt pat body
            altWild  = Alt PWild (EVar (BC.pack "[]"))
            fn = ELam var (ECase (EVar var) [altMatch, altWild])
        in EApp (EApp (EVar (BC.pack "concatMap")) fn) src
    qualWrap (QGuard g) body =
        EIf g body (EVar (BC.pack "[]"))
    qualWrap (QLet binds) body = ELet binds body

-- | One list-comprehension qualifier.
data Qual
    = QGen   !Pat !Expr        -- ^ @pat <- expr@
    | QGuard !Expr              -- ^ @condition@
    | QLet   ![Bind]            -- ^ @let bindings@

parseQual :: Ctx -> Cursor -> IO (Qual, Cursor)
parseQual ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkLet -> do
            -- @let@ qualifier inside a list comprehension: collect all
            -- same-column name-bindings and stop at the enclosing `,`
            -- or `]` of the comprehension.  No `in` keyword follows —
            -- that's the key difference from a normal `let … in …`.
            (binds, curE) <- parseLetQualBinds ctx cur1
            pure (QLet binds, curE)
        _ -> do
            -- Try pattern <- expr first; else treat as guard.
            mGen <- tryGen ctx cur0
            case mGen of
                Just (pat, srcE, curEnd) -> pure (QGen pat srcE, curEnd)
                Nothing -> do
                    (e, curE) <- parseExpr ctx cur0
                    pure (QGuard e, curE)

-- | Parse the bindings of a @let@ qualifier inside a list
-- comprehension.  Bindings are collected by layout: the first binding
-- establishes a column, each additional binding at that column is
-- accumulated, and the loop ends at `,`/`]` (the enclosing list-comp
-- separators) or EOF.  RHS expressions use @ctxMinCol = bindCol@ so
-- the expression parser stops at a same-column sibling binding.
parseLetQualBinds :: Ctx -> Cursor -> IO ([Bind], Cursor)
parseLetQualBinds ctx cur0 = do
    let (firstTok, _) = nextSig ctx cur0
    case tkKind firstTok of
        TkEof      -> pure ([], cur0)
        TkComma    -> pure ([], cur0)
        TkRBracket -> pure ([], cur0)
        _ -> do
            let bindCol = tkCol firstTok
            loop bindCol cur0 []
  where
    loop bindCol cur acc = do
        (bind, cur') <- parseOneQualBind bindCol cur
        let acc' = bind : acc
            (peek, _) = nextSig ctx cur'
        case tkKind peek of
            TkComma    -> pure (reverse acc', cur')
            TkRBracket -> pure (reverse acc', cur')
            TkEof      -> pure (reverse acc', cur')
            _ | tkCol peek == bindCol -> loop bindCol cur' acc'
              | otherwise             -> pure (reverse acc', cur')

    parseOneQualBind bindCol cur = do
        let (nameTok, cur1) = nextSig ctx cur
            rhsCtx = ctx { ctxMinCol = max (ctxMinCol ctx) bindCol }
        case tkKind nameTok of
            TkIdent n -> do
                (params, cur2) <- collectLetParams ctx cur1 []
                let (sepTok, cur3) = nextSig ctx cur2
                case tkKind sepTok of
                    TkEq -> do
                        (e, cur4) <- parseExpr rhsCtx cur3
                        pure ((n, wrapParams params e), cur4)
                    _ -> parseErr ctx "expected `=` in list-comp let-binding" sepTok
            _ -> parseErr ctx "expected identifier in list-comp let-binding" nameTok

-- | Try to parse @pat <- expr@.  Returns Nothing if the input doesn't
-- match (we'll then fall back to parsing it as a guard expression).
tryGen :: Ctx -> Cursor -> IO (Maybe (Pat, Expr, Cursor))
tryGen ctx cur0 = do
    r <- try (do
                (pat, cur1) <- parseTopPat ctx cur0
                let (arrow, cur2) = nextSig ctx cur1
                case tkKind arrow of
                    TkLArrow -> do
                        (e, cur3) <- parseExpr ctx cur2
                        pure (Just (pat, e, cur3))
                    _ -> pure Nothing) :: IO (Either ParseError (Maybe (Pat, Expr, Cursor)))
    case r of
        Right v  -> pure v
        Left  _  -> pure Nothing

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

-- | Skip past the body of a QuasiQuoter @[name|…|]@ at the BYTE level
-- (rather than the token level) because QQ bodies can contain
-- arbitrary characters (HTML in HSX, SQL, interpolated text) that do
-- not tokenise cleanly as Haskell.  Tracks nesting of inner
-- @[name|…|]@ forms so nested HSX fragments work.  Returns the cursor
-- just past the matching @|]@.
skipQQBody :: Ctx -> Cursor -> IO Cursor
skipQQBody ctx cur0 = pure (go cur0 (1 :: Int))
  where
    src = ctxSrc ctx

    -- Walk forward one byte, bumping line/col.
    step1 c = case peekByte src (cPos c) of
        Just 0x0A -> Cursor (cPos c + 1) (cLine c + 1) 1
        _         -> Cursor (cPos c + 1) (cLine c)     (cCol c + 1)

    -- Walk forward by N bytes (no newlines expected in the N chars).
    stepN n c = Cursor (cPos c + n) (cLine c) (cCol c + n)

    -- @depth@ counts open QQs we've entered but not closed.  Start at 1
    -- because the caller has just consumed the @[name|@ opener.
    go c !depth
        | depth == 0                      = c
        | otherwise = case peekByte src (cPos c) of
            Nothing -> c   -- EOF: bail, parser will error downstream if needed
            -- @|]@ closes one QQ layer.
            Just 0x7C | peekByte src (cPos c + 1) == Just 0x5D ->
                go (stepN 2 c) (depth - 1)
            -- @[name|@ opens a nested QQ.
            Just 0x5B ->
                case matchNestedQQ (cPos c + 1) of
                    Just endPos ->
                        go (stepN (endPos - cPos c) c) (depth + 1)
                    Nothing -> go (step1 c) depth
            _ -> go (step1 c) depth

    -- Detect a nested QQ opener at byte position @p@ (which is the byte
    -- after a @[@).  Returns the position just past the @|@ of
    -- @[name|@, or Nothing if this bracket isn't a QQ.
    matchNestedQQ p = do
        firstByte <- peekByte src p
        if isLowerStartByte firstByte
            then
                let end = runIdent p
                in case peekByte src end of
                    Just 0x7C -> Just (end + 1)   -- [name|
                    _         -> Nothing
            else Nothing

    runIdent p = case peekByte src p of
        Just b | isIdentContByte b -> runIdent (p + 1)
        _                          -> p

    isLowerStartByte b = (b >= 0x61 && b <= 0x7A) || b == 0x5F   -- a..z or _
    isIdentContByte b =
           (b >= 0x61 && b <= 0x7A)       -- a..z
        || (b >= 0x41 && b <= 0x5A)       -- A..Z
        || (b >= 0x30 && b <= 0x39)       -- 0..9
        || b == 0x5F                      -- _
        || b == 0x27                      -- '

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
