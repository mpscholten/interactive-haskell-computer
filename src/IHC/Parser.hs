-- | Single-pass recursive-descent parser for binding bodies.
--
-- Does not build an AST. As it parses tokens, it calls into the
-- 'CodeBuffer' to emit aarch64 instructions directly. The buffer must
-- already be writable when 'parseBody' is called.
--
-- Phase 1.0 grammar:
--
--   body ::= int-literal          -- emits: movz/movk chain into x0 ; ret
--
-- Later slices add binary ops, function application, class methods,
-- and TH quotation. Adding each is a matter of extending 'parseExpr'
-- with a new branch that emits more instructions.
{-# LANGUAGE DeriveAnyClass #-}

module IHC.Parser
    ( parseBody
    , ParseError(..)
    ) where

import Control.Exception (Exception, throwIO)

import IHC.CodeBuffer
import IHC.Encode
import IHC.Lexer
import IHC.Source

newtype ParseError = ParseError String
    deriving stock (Show)
    deriving anyclass (Exception)

-- | Parse the body of a top-level binding whose text spans the given
-- byte range, emitting the corresponding aarch64 code into @cb@.
--
-- The caller is responsible for positioning the 'CodeBuffer' where the
-- entry point should land and for being inside a 'withWritable'
-- bracket.
parseBody :: Source -> Span -> CodeBuffer -> IO ()
parseBody src (start, end) cb = do
    let cur0 = Cursor start 1 1
    (_tok, cur1) <- parseExpr src (end, cur0) cb
    -- Terminate with RET. We don't care whether cur1 reached `end` exactly —
    -- trailing whitespace / comments are benign.
    _ <- pure cur1
    emitInsn cb retX30

-- | Parse a single expression, emitting code that leaves its value in @x0@.
-- Returns the token cursor past the expression.
parseExpr :: Source -> (Pos, Cursor) -> CodeBuffer -> IO (Token, Cursor)
parseExpr src (endPos, cur0) cb = do
    let (tok, cur1) = nextToken src cur0
    case tkKind tok of
        TkInt n -> do
            -- Emit `mov x0, #n` (or the movz/movk chain for larger values).
            emitInsns cb (loadInt64 0 (fromInteger n))
            pure (tok, cur1)
        TkEof ->
            throwIO (ParseError ("empty body at offset " <> show (tkStart tok)))
        _ ->
            throwIO (ParseError ("expected Int literal at offset "
                                 <> show (tkStart tok)
                                 <> " but saw " <> show (tkKind tok)))
  -- Suppress unused-var warning.
  where _ = endPos
