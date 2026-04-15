{-# LANGUAGE DeriveAnyClass #-}

-- | Single-pass recursive-descent parser for binding bodies.
--
-- Parses the body once, producing a small 'Item' list. No AST; items
-- map 1:1 to tiny instruction groups. This decoupling from the code
-- buffer lets the scheduler safely recur into dependency compilation
-- without scrambling bump-pointer layout.
--
-- Grammar (Phase 1.3):
--
-- @
-- body  ::= expr
-- expr  ::= term (('+' | '-') term)*       -- left-assoc
-- term  ::= atom ('*' atom)*               -- left-assoc, tighter
-- atom  ::= INT
--         | ident                           -- param OR nullary call
--         | ident INT                       -- one-arg call with literal arg
-- @
--
-- The parser takes the enclosing binding's parameter list so it can
-- tell local parameter references from top-level calls.
module IHC.Parser
    ( parseBodyItems
    , ParseError(..)
    ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)

import IHC.IR
import IHC.Lexer
import IHC.Source

newtype ParseError = ParseError String
    deriving stock (Show)
    deriving anyclass (Exception)

-- | Parse a binding body's span. @params@ is the enclosing binding's
-- parameter list (zero or one entry in Phase 1.3).
parseBodyItems :: Source -> [ByteString] -> Span -> IO [Item]
parseBodyItems src params (start, _end) = do
    let cur0 = Cursor start 1 1
    (items, _cur1) <- parseExpr src params cur0 []
    pure (reverse items)

parseExpr :: Source -> [ByteString] -> Cursor -> [Item] -> IO ([Item], Cursor)
parseExpr src params cur0 acc0 = do
    (acc1, cur1) <- parseTerm src params cur0 acc0
    loop acc1 cur1
  where
    loop acc cur =
        let (tok, curN) = nextToken src cur in
        case tkKind tok of
            TkPlus -> do
                (acc', cur') <- parseTerm src params curN (IPushX0 : acc)
                loop (IAddX1X0 : IPopX1 : acc') cur'
            TkMinus -> do
                (acc', cur') <- parseTerm src params curN (IPushX0 : acc)
                loop (ISubX1X0 : IPopX1 : acc') cur'
            _ -> pure (acc, cur)

parseTerm :: Source -> [ByteString] -> Cursor -> [Item] -> IO ([Item], Cursor)
parseTerm src params cur0 acc0 = do
    (acc1, cur1) <- parseAtom src params cur0 acc0
    loop acc1 cur1
  where
    loop acc cur =
        let (tok, curN) = nextToken src cur in
        case tkKind tok of
            TkStar -> do
                (acc', cur') <- parseAtom src params curN (IPushX0 : acc)
                loop (IMulX1X0 : IPopX1 : acc') cur'
            _ -> pure (acc, cur)

parseAtom :: Source -> [ByteString] -> Cursor -> [Item] -> IO ([Item], Cursor)
parseAtom src params cur0 acc0 = do
    let (tok, cur1) = nextToken src cur0
    case tkKind tok of
        TkInt n ->
            pure (ILitInt (fromInteger n) : acc0, cur1)
        TkIdent name
            | name `elem` params ->
                pure (IArg : acc0, cur1)
            | otherwise -> do
                -- Look for a 1-arg call: `f <literal>` or `f <param>`.
                let (nextTok, cur2) = nextToken src cur1
                case tkKind nextTok of
                    TkInt k ->
                        pure (ICall1 name : ILitInt (fromInteger k) : acc0, cur2)
                    TkIdent arg
                        | arg `elem` params ->
                            pure (ICall1 name : IArg : acc0, cur2)
                    _ ->
                        pure (ICall name : acc0, cur1)
        TkEof ->
            throwIO (ParseError ("empty expression at offset " <> show (tkStart tok)))
        other ->
            throwIO (ParseError ("expected Int literal or identifier at offset "
                                 <> show (tkStart tok)
                                 <> " but saw " <> show other))
