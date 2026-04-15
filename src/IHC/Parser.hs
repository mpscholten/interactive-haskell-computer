{-# LANGUAGE DeriveAnyClass #-}

-- | Single-pass recursive-descent parser for binding bodies.
--
-- Parses the body's bytes once, producing a small 'Item' list. No AST;
-- items map 1:1 to tiny instruction groups that the emitter later
-- expands into aarch64. This decoupling from the code buffer is what
-- lets the scheduler safely recur into dependency compilation without
-- scrambling bump-pointer layout.
--
-- Grammar (Phase 1.2):
--
-- @
-- body  ::= expr
-- expr  ::= term (('+' | '-') term)*       -- left-associative
-- term  ::= atom ('*' atom)*               -- left-associative, tighter
-- atom  ::= INT
--         | ident                           -- refers to another binding
-- @
module IHC.Parser
    ( parseBodyItems
    , ParseError(..)
    ) where

import Control.Exception (Exception, throwIO)

import IHC.IR
import IHC.Lexer
import IHC.Source

newtype ParseError = ParseError String
    deriving stock (Show)
    deriving anyclass (Exception)

-- | Parse a binding body's span, returning its 'Item' list.
parseBodyItems :: Source -> Span -> IO [Item]
parseBodyItems src (start, _end) = do
    let cur0 = Cursor start 1 1
    (items, _cur1) <- parseExpr src cur0 []
    pure (reverse items)

-- | Parse an expression. Items are accumulated in reverse for O(1)
-- prepends; the caller reverses once at the top.
parseExpr :: Source -> Cursor -> [Item] -> IO ([Item], Cursor)
parseExpr src cur0 acc0 = do
    (acc1, cur1) <- parseTerm src cur0 acc0
    loop acc1 cur1
  where
    loop acc cur =
        let (tok, curN) = nextToken src cur in
        case tkKind tok of
            TkPlus -> do
                (acc', cur') <- parseTerm src curN (IPushX0 : acc)
                loop (IAddX1X0 : IPopX1 : acc') cur'
            TkMinus -> do
                (acc', cur') <- parseTerm src curN (IPushX0 : acc)
                loop (ISubX1X0 : IPopX1 : acc') cur'
            _ -> pure (acc, cur)

parseTerm :: Source -> Cursor -> [Item] -> IO ([Item], Cursor)
parseTerm src cur0 acc0 = do
    (acc1, cur1) <- parseAtom src cur0 acc0
    loop acc1 cur1
  where
    loop acc cur =
        let (tok, curN) = nextToken src cur in
        case tkKind tok of
            TkStar -> do
                (acc', cur') <- parseAtom src curN (IPushX0 : acc)
                loop (IMulX1X0 : IPopX1 : acc') cur'
            _ -> pure (acc, cur)

parseAtom :: Source -> Cursor -> [Item] -> IO ([Item], Cursor)
parseAtom src cur0 acc0 = do
    let (tok, cur1) = nextToken src cur0
    case tkKind tok of
        TkInt n ->
            pure (ILitInt (fromInteger n) : acc0, cur1)
        TkIdent name ->
            pure (ICall name : acc0, cur1)
        TkEof ->
            throwIO (ParseError ("empty expression at offset " <> show (tkStart tok)))
        other ->
            throwIO (ParseError ("expected Int literal or identifier at offset "
                                 <> show (tkStart tok)
                                 <> " but saw " <> show other))
