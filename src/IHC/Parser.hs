{-# LANGUAGE DeriveAnyClass #-}

-- | Single-pass recursive-descent parser producing AST.
--
-- Same surface grammar as Phase-1.13. Output is now @Expr@ from
-- 'IHC.AST' instead of an Item list. Multi-arg functions desugar to
-- nested lambdas: @f x y = e@ becomes @"f" -> ELam "x" (ELam "y" e)@.
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

-- | Parse the body of a top-level binding. The @params@ list comes
-- from the LHS scan and becomes a chain of lambdas around the body.
parseBodyExpr :: Source -> [Name] -> Span -> IO Expr
parseBodyExpr src params span_ = do
    let (bodySpan, mWhere) = splitOnWhere src span_
    whereBinds <- case mWhere of
        Nothing -> pure []
        Just ws -> parseBindingsIn src ws
    let cur0 = Cursor (fst bodySpan) 1 1
        -- Top-level bindings start at column 1; the body's expression
        -- can therefore freely span any column > 1 but must NOT
        -- absorb a column-1 token (that would be the next binding).
        ctx  = Ctx src (snd bodySpan) 1
    (bodyExpr, _) <- parseExpr ctx cur0
    let core = case whereBinds of
            [] -> bodyExpr
            bs -> ELet bs bodyExpr
    pure (foldr ELam core params)

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
        let (patTok, cur1) = nextSig ctx cur
        pat <- case tkKind patTok of
            TkInt n      -> pure (PLit (LInt (fromInteger n)))
            TkStr s      -> pure (PLit (LStr s))
            TkUnderscore -> pure PWild
            TkIdent n    -> pure (PVar n)
            _            -> parseErr "expected pattern (Int, String, _, or ident) in case alt" patTok
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
-- Operator precedence
--------------------------------------------------------------------------------

parseOr, parseAnd, parseRel, parseSum, parseTerm, parseApp, parseAtom
    :: Ctx -> Cursor -> IO (Expr, Cursor)

parseOr  ctx c = chain1L ctx c parseAnd "||" matchesOr
  where matchesOr TkOr = True; matchesOr _ = False
parseAnd ctx c = chain1L ctx c parseRel "&&" matchesAnd
  where matchesAnd TkAnd = True; matchesAnd _ = False

parseRel ctx cur0 = do
    (l, cur1) <- parseSum ctx cur0
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
        (r, cur') <- parseSum ctx cur
        pure (EApp (EApp (EVar opName) l) r, cur')

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
    startsAtom TkLParen       = True
    startsAtom TkIdent{}      = True
    startsAtom _              = False

parseAtom ctx cur0 = do
    let (tok, cur1) = nextSig ctx cur0
    case tkKind tok of
        TkInt n  -> pure (ELit (LInt (fromInteger n)), cur1)
        TkStr s  -> pure (ELit (LStr s), cur1)
        TkIdent n
            | n == "_" -> parseErr "wildcard `_` in expression position" tok
            | otherwise -> pure (EVar n, cur1)
        TkLParen -> do
            (e, cur2) <- parseExpr ctx cur1
            let (close, cur3) = nextSig ctx cur2
            case tkKind close of
                TkRParen -> pure (e, cur3)
                _        -> parseErr "expected `)`" close
        TkMinus -> do
            (e, cur2) <- parseAtom ctx cur1
            pure (ENeg e, cur2)
        TkEof -> throwIO (ParseError ("empty expression at offset " <> show (tkStart tok)))
        _ -> parseErr "unexpected token" tok

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
