{-# LANGUAGE OverloadedStrings #-}

-- | Source-level pretty-printer for the parser AST.
--
-- The contract is a /parser inverse/: for every 'Expr' the
-- generator in @test/Properties/Generators.hs@ can reach,
--
-- @
-- parseExprAtEof (prettyExpr e) defaultFixityTable  ===  Right e
-- @
--
-- (modulo the AST normalisation in @test/Properties/RoundTrip.hs@).
-- Defensive parens are emitted at every operator boundary; this is
-- a machine-readable form, not a human-readable one.
--
-- Phase 2 of the property-based testing plan rolls the AST out
-- sub-language at a time: literals first, then 'EVar' / 'EApp' /
-- 'ELam' / 'ELet', then control flow, then patterns, …  See
-- @plans/can-we-convert-the-temporal-castle.md@.  The first slice
-- in this module covers just 'LInt' \/ 'ELit'; subsequent commits
-- extend coverage and the @prop_acceptance@ \/ @prop_roundtrip@
-- properties grow with it.
--
-- Internal-only AST nodes ('ETypedMethod', 'EGuardFail') are
-- excluded by construction — the generator never produces them and
-- 'prettyExpr' raises a clear @error@ if it sees one.
module IHC.Pretty
    ( prettyExpr
    , prettyLit
    , prettyPat
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (chr, ord)
import Data.Word (Word8)

import IHC.AST


-- | Pretty-print an 'Expr' to source bytes the parser will accept.
--
-- Constructors the generator does not yet produce raise an
-- explicit @error@ so a future generator extension that forgets
-- to update this function fails loudly instead of silently
-- emitting bogus source.
--
-- Slice 2.D additionally covers 'EVar' \/ 'EApp' \/ 'ELam' \/
-- 'ELet'.  Defensive parens wrap every non-atom: 'EApp' becomes
-- @(f x)@, 'ELam' becomes @(\\x -> body)@, 'ELet' becomes
-- @(let { x = e1; y = e2 } in body)@.  Atoms ('EVar', 'ELit')
-- are emitted bare — they are unambiguous tokens.
prettyExpr :: Expr -> ByteString
prettyExpr = \case
    ELit  l        -> prettyLit l
    EVar  n        -> n
    EApp  f x      ->
        "(" <> prettyExpr f <> " " <> prettyExpr x <> ")"
    ELam  x body   ->
        "(\\" <> x <> " -> " <> prettyExpr body <> ")"
    ELet  binds b  ->
        "(let { " <> prettyBinds binds <> " } in " <> prettyExpr b <> ")"
    EIf   c t e    ->
        "(if " <> prettyExpr c
          <> " then " <> prettyExpr t
          <> " else " <> prettyExpr e <> ")"
    ECase scrut alts ->
        "(case " <> prettyExpr scrut
          <> " of { " <> bsIntercalate "; " (map prettyAlt alts)
          <> " })"
    EDo   stmts    ->
        "(do { " <> bsIntercalate "; " (map prettyStmt stmts) <> " })"
    ENeg  e        ->
        -- The space after @-@ is defensive: without it,
        -- @-3@ is fine but @-x@ next to certain operators in
        -- non-paren contexts could lex differently.  The outer
        -- parens isolate the unary form from any surrounding
        -- expression so re-parse always reaches 'parseUnary'.
        "(- " <> prettyExpr e <> ")"
    ETuple es      ->
        "(" <> bsIntercalate ", " (map prettyExpr es) <> ")"
    ELabel n       -> "#" <> n
    EImplicitRef n -> "?" <> n
    -- @f \@T@ — value-level TypeApplications.  The Name field is
    -- the raw source bytes of the type argument as captured by
    -- 'IHC.Parser.captureTypeArg'.  For a bare ConId the bytes
    -- are just the ConId text; parenthesised forms
    -- (@\@(Maybe Int)@) carry the parens too, which the
    -- generator does not yet emit.
    ETyApp e n     -> "(" <> prettyExpr e <> " @" <> n <> ")"
    -- @let ?x = e1; ?y = e2 in body@.  Bindings carry the
    -- implicit-param name /without/ the leading @?@ — that
    -- prefix is part of the syntax, not the AST name.  See
    -- 'IHC.Parser.parseOneIPBind'.
    EImplicitLet binds b ->
        "(let { " <> bsIntercalate "; " (map prettyImpBind binds)
          <> " } in " <> prettyExpr b <> ")"
    -- Record construction: @Con { f1 = e1, f2 = e2 }@.  The
    -- parser disambiguates record-construction from
    -- record-update by checking whether the head is a
    -- 'recordConstructorName' (uppercase-starting) — see
    -- @IHC.Parser:3282@.
    ERecordCon n fields ->
        "(" <> n <> " { "
          <> bsIntercalate ", " (map prettyFieldExpr fields)
          <> " })"
    -- @Con {..}@ — RecordWildCards.  Parser side at
    -- @IHC.Parser:3286@: parseRecordFields detects @..@ and
    -- returns isWild=True.
    ERecordWild n  ->
        "(" <> n <> " {..})"
    -- Record update: @<expr> { f1 = e1, f2 = e2 }@.  Parsed by
    -- the parseApp loop at @IHC.Parser:3043@; the head can be
    -- any 'Expr', which is what 'isRecordUpdateBrace' lets us
    -- distinguish from record /construction/ (where the head
    -- must be a 'TkConId').
    ERecordUpdate e fields ->
        "(" <> prettyExpr e <> " { "
          <> bsIntercalate ", " (map prettyFieldExpr fields)
          <> " })"
    e              -> error
        ( "IHC.Pretty.prettyExpr: unsupported Expr constructor.\n"
          <> "  Phase 2 generators are bounded to constructors that\n"
          <> "  prettyExpr handles; extend both together.\n"
          <> "  Got: " <> takeShow 120 e )


-- | Pretty-print a single 'case' alternative as @<pat> -> <body>@.
-- Guards land alongside richer pattern coverage in a later slice.
prettyAlt :: Alt -> ByteString
prettyAlt (Alt p body) = prettyPat p <> " -> " <> prettyExpr body


-- | Pretty-print a single 'do'-block statement.
--
-- @SBangBind@ \/ @SImplicitLet@ are deferred until BangPatterns
-- and ImplicitParams enter the generator; an explicit @error@
-- here keeps generator + pretty in lockstep.
prettyStmt :: Stmt -> ByteString
prettyStmt = \case
    SExpr e        -> prettyExpr e
    SBind n e      -> n <> " <- " <> prettyExpr e
    SLet  binds    -> "let { " <> prettyBinds binds <> " }"
    s              -> error
        ( "IHC.Pretty.prettyStmt: unsupported Stmt constructor.\n"
          <> "  Got: " <> takeShow 80 s )


-- | Pretty-print a 'Pat' to source bytes.  Slice 2.F covers the
-- bulk of the pattern grammar ('PVar', 'PWild', 'PLit', 'PCon',
-- 'PTuple', 'PAs', 'PBang', 'PIrref').  The record-shaped
-- constructors ('PRecord', 'PRecordWild', 'PView') land in a
-- follow-up.
--
-- Defensive parens wrap every non-atom pattern.  Atoms ('PVar',
-- 'PWild', 'PLit') emit bare.  'PTuple' is its own paren-bearing
-- form, so we don't double-wrap it.
prettyPat :: Pat -> ByteString
prettyPat = \case
    PWild           -> "_"
    PVar n          -> n
    PLit l          -> prettyLit l
    PCon n args     ->
        "(" <> n <> argsPart <> ")"
      where
        argsPart
          | null args = BS.empty
          | otherwise = " " <> bsIntercalate " " (map prettyPat args)
    PTuple ps       ->
        "(" <> bsIntercalate ", " (map prettyPat ps) <> ")"
    PAs n p         -> n <> "@(" <> prettyPat p <> ")"
    PBang p         -> "(!(" <> prettyPat p <> "))"
    PIrref p        -> "(~(" <> prettyPat p <> "))"
    p               -> error
        ( "IHC.Pretty.prettyPat: unsupported Pat constructor.\n"
          <> "  Slice 2.F covers PVar / PWild / PLit / PCon /\n"
          <> "  PTuple / PAs / PBang / PIrref.  PRecord /\n"
          <> "  PRecordWild / PView land in a follow-up.\n"
          <> "  Got: " <> takeShow 80 p )


-- | Pretty-print a 'let'-binding group.  Bindings are joined by
-- @\"; \"@ and the whole group is wrapped in @\"{ ... }\"@ at the
-- 'ELet' call site.  Explicit braces are used in lieu of the
-- offside rule so the pretty-printer does not have to reason about
-- column boundaries.
prettyBinds :: [Bind] -> ByteString
prettyBinds = bsIntercalate "; " . map prettyBind


prettyBind :: Bind -> ByteString
prettyBind (n, e) = n <> " = " <> prettyExpr e


-- | Pretty-print one implicit-let binding (@?name = expr@).
-- 'EImplicitLet' carries the implicit-param name without the
-- leading @?@; we add it back here so the parser's
-- 'parseOneIPBind' recognises the binding.
prettyImpBind :: (Name, Expr) -> ByteString
prettyImpBind (n, e) = "?" <> n <> " = " <> prettyExpr e


-- | Pretty-print one @field = expr@ entry inside an
-- 'ERecordCon' or 'ERecordUpdate' brace block.
prettyFieldExpr :: (Name, Expr) -> ByteString
prettyFieldExpr (n, e) = n <> " = " <> prettyExpr e


-- | Local helper — 'BS.intercalate' is in @Data.ByteString@ but
-- we only have @Data.ByteString.Char8@ in scope, and importing
-- one extra module just for this is heavier than spelling it out.
bsIntercalate :: ByteString -> [ByteString] -> ByteString
bsIntercalate _   []       = BS.empty
bsIntercalate _   [b]      = b
bsIntercalate sep (b : bs) = b <> sep <> bsIntercalate sep bs

-- | Pretty-print a 'Lit' to its source-level form.
--
-- All literals here are emitted unsigned: the parser lexes @-5@ as
-- @TkMinus@ + @TkInt 5@ and parses it as @ENeg (ELit (LInt 5))@,
-- so negative numerics are produced via 'ENeg' at the generator
-- level (matching the parser's actual AST shape) — never as a
-- negative payload inside 'LInt' \/ 'LInteger' \/ 'LFloat'.
prettyLit :: Lit -> ByteString
prettyLit = \case
    LInt     n -> BC.pack (show n)
    LInteger n -> BC.pack (show n)
    -- 'show' for 'Double' is round-trippable per Haskell Report
    -- (read . show === id) and emits decimal forms ("0.0", "1.5",
    -- "1.0e-3") that 'IHC.Lexer.lexFloat' accepts.  Generator
    -- excludes NaN \/ Infinity so this branch stays inside the
    -- finite-double subset the parser handles.
    LFloat   d -> BC.pack (show d)
    LChar    c -> "'" <> BC.pack (escapeCharLit c) <> "'"
    LStr    bs -> "\"" <> BS.concatMap escapeStrByte bs <> "\""


-- | Escape a 'Char' for use inside a single-quote 'LChar' literal.
-- Printable ASCII (except @'@, @\\@, and the three TkTick
-- triggers — see below) is emitted verbatim; the standard
-- whitespace escapes ('\n' \/ '\t' \/ '\r') get their named
-- forms; everything else (control bytes, non-ASCII Unicode up to
-- 'maxBound :: Char') uses the @\\<decimal>@ form, which the
-- lexer accepts up to 0x10FFFF (see @test/ParserBugs.hs:55@).
--
-- Three printable-ASCII chars need numeric escaping despite
-- otherwise being legal char-literal contents: @(@, @[@, @{@.
-- The lexer disambiguates @\'(@ \/ @\'[@ \/ @\'{@ as a
-- 'IHC.Lexer.TkTick' (DataKinds-style promoted-tuple\/list\/
-- record literal), not as the start of a 'Char' literal — see
-- @src/IHC/Lexer.hs:618-635@.  Without escaping, @prettyLit
-- (LChar '(') = "'('"@ would be lexed as @TkTick TkLParen
-- TkChar@ and the parser would reject it.
escapeCharLit :: Char -> String
escapeCharLit c
    | c == '\\'                    = "\\\\"
    | c == '\''                    = "\\'"
    | c == '\n'                    = "\\n"
    | c == '\t'                    = "\\t"
    | c == '\r'                    = "\\r"
    -- TkTick disambiguation triggers — must escape numerically.
    | c == '('                     = "\\" <> show (ord c)
    | c == '['                     = "\\" <> show (ord c)
    | c == '{'                     = "\\" <> show (ord c)
    | ord c >= 0x20 && ord c < 0x7F = [c]            -- printable ASCII
    | otherwise                    = "\\" <> show (ord c)


-- | Escape a single byte for use inside a 'LStr' double-quote
-- literal.  Always appends @\\&@ after a numeric escape so the
-- next character (whatever it is) cannot extend the digit run —
-- @\\65A@ is unambiguous, but @\\65@ followed by another digit
-- @5@ in source would re-parse as @\\655@.  @\\&@ is a
-- zero-width separator that solves it cheaply.
escapeStrByte :: Word8 -> ByteString
escapeStrByte b
    | b == 0x5C                = "\\\\"           -- '\\'
    | b == 0x22                = "\\\""           -- '\"'
    | b == 0x0A                = "\\n"
    | b == 0x09                = "\\t"
    | b == 0x0D                = "\\r"
    | b >= 0x20 && b < 0x7F    = BS.singleton b   -- printable ASCII
    | otherwise                = BC.pack ("\\" <> show b <> "\\&")


-- | Truncate a 'Show' rendering to keep error messages bounded
-- when an unhandled constructor appears.
takeShow :: Show a => Int -> a -> String
takeShow n x =
    let s = show x
    in if length s <= n then s else take (n - 1) s <> "…"
