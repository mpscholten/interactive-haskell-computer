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
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import IHC.AST


-- | Pretty-print an 'Expr' to source bytes the parser will accept.
--
-- Phase 2.A first slice: only 'ELit' is reachable.  Constructors
-- the generator does not yet produce raise an explicit @error@ so
-- a future generator extension that forgets to update this
-- function fails loudly instead of silently emitting bogus source.
prettyExpr :: Expr -> ByteString
prettyExpr = \case
    ELit l -> prettyLit l
    e      -> error
        ( "IHC.Pretty.prettyExpr: unsupported Expr constructor.\n"
          <> "  Phase 2 generators are bounded to constructors that\n"
          <> "  prettyExpr handles; extend both together.\n"
          <> "  Got: " <> takeShow 120 e )

-- | Pretty-print a 'Lit' to its source-level form.
--
-- All literals here are emitted unsigned: the parser lexes @-5@ as
-- @TkMinus@ + @TkInt 5@ and parses it as @ENeg (ELit (LInt 5))@,
-- so negative numerics are produced via 'ENeg' at the generator
-- level (matching the parser's actual AST shape) — never as a
-- negative payload inside 'LInt' \/ 'LInteger' \/ 'LFloat'.
prettyLit :: Lit -> ByteString
prettyLit = \case
    LInt n     -> BC.pack (show n)
    -- Subsequent slices add LInteger / LFloat / LChar / LStr.
    -- They are intentionally absent from the first slice so the
    -- generator can be bounded to a single literal flavour while
    -- the round-trip pipeline is validated end-to-end.
    l          -> error
        ( "IHC.Pretty.prettyLit: unsupported Lit constructor.\n"
          <> "  Phase 2.A generates only LInt; extend pretty +\n"
          <> "  generator together when broadening Lit coverage.\n"
          <> "  Got: " <> takeShow 80 l )


-- | Truncate a 'Show' rendering to keep error messages bounded
-- when an unhandled constructor appears.
takeShow :: Show a => Int -> a -> String
takeShow n x =
    let s = show x
    in if length s <= n then s else take (n - 1) s <> "…"
