{-# LANGUAGE DeriveAnyClass #-}

-- | Single-pass recursive-descent parser for binding bodies.
--
-- Does not build an AST. As it parses tokens, it calls into the
-- 'CodeBuffer' to emit aarch64 instructions directly. Each subexpression
-- leaves its value in @x0@; binary operators push the left operand to
-- the stack, evaluate the right, then pop and combine.
--
-- Grammar (Phase 1.1):
--
-- @
-- body  ::= expr
-- expr  ::= term (('+' | '-') term)*      -- left-associative
-- term  ::= atom ('*' atom)*              -- left-associative, tighter
-- atom  ::= INT
-- @
--
-- Adding new forms (function application, class methods, TH, \...) is a
-- matter of extending 'parseAtom' with new branches that emit more
-- instructions.
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
parseBody src (start, _end) cb = do
    let cur0 = Cursor start 1 1
    _ <- parseExpr src cur0 cb
    emitInsn cb retX30

-- | Parse and emit code for an expression; returns the cursor positioned
-- at (or past) the first token that wasn't part of the expression.
parseExpr :: Source -> Cursor -> CodeBuffer -> IO Cursor
parseExpr src cur0 cb = do
    cur1 <- parseTerm src cur0 cb
    loop cur1
  where
    loop cur =
        let (tok, curN) = nextToken src cur in
        case tkKind tok of
            TkPlus -> do
                emitInsn cb pushX0                -- save left
                cur2 <- parseTerm src curN cb     -- right → x0
                emitInsn cb popX1                 -- left → x1
                emitInsn cb (addXXX 0 1 0)        -- x0 = x1 + x0
                loop cur2
            TkMinus -> do
                emitInsn cb pushX0
                cur2 <- parseTerm src curN cb
                emitInsn cb popX1
                emitInsn cb (subXXX 0 1 0)        -- x0 = x1 - x0
                loop cur2
            _ -> pure cur                          -- leave tok unconsumed

parseTerm :: Source -> Cursor -> CodeBuffer -> IO Cursor
parseTerm src cur0 cb = do
    cur1 <- parseAtom src cur0 cb
    loop cur1
  where
    loop cur =
        let (tok, curN) = nextToken src cur in
        case tkKind tok of
            TkStar -> do
                emitInsn cb pushX0
                cur2 <- parseAtom src curN cb
                emitInsn cb popX1
                emitInsn cb (mulXXX 0 1 0)        -- x0 = x1 * x0
                loop cur2
            _ -> pure cur

parseAtom :: Source -> Cursor -> CodeBuffer -> IO Cursor
parseAtom src cur0 cb = do
    let (tok, cur1) = nextToken src cur0
    case tkKind tok of
        TkInt n -> do
            emitInsns cb (loadInt64 0 (fromInteger n))
            pure cur1
        TkEof ->
            throwIO (ParseError ("empty expression at offset " <> show (tkStart tok)))
        other ->
            throwIO (ParseError ("expected Int literal at offset "
                                 <> show (tkStart tok)
                                 <> " but saw " <> show other))
