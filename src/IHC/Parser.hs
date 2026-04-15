{-# LANGUAGE DeriveAnyClass #-}

-- | Single-pass recursive-descent parser for binding bodies.
--
-- Parses the body once, producing a small 'Item' list. No AST.
--
-- Grammar (Phase 1.4):
--
-- @
-- body  ::= expr
-- expr  ::= 'if' expr 'then' expr 'else' expr
--         | rel
-- rel   ::= sum  ('<=' sum)?            -- only one comparison; not chained
-- sum   ::= term (('+' | '-') term)*    -- left-assoc
-- term  ::= atom ('*' atom)*            -- left-assoc
-- atom  ::= INT
--         | '(' expr ')'
--         | ident                        -- param OR nullary call
--         | ident atomArg                -- one-arg call: arg is an atom
-- atomArg ::= INT | ident | '(' expr ')' -- whatever counts as a "tight" argument
-- @
--
-- The parser takes the enclosing binding's parameter list so it can
-- distinguish local parameter references from top-level calls.
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
-- parameter list (zero or one entry in Phase 1.3+).
parseBodyItems :: Source -> [ByteString] -> Span -> IO [Item]
parseBodyItems src params (start, _end) = do
    let cur0 = Cursor start 1 1
    (items, _cur1) <- parseExpr src params cur0 []
    pure (reverse items)

-- top-level expression: handles 'if'.
parseExpr :: Source -> [ByteString] -> Cursor -> [Item] -> IO ([Item], Cursor)
parseExpr src params cur0 acc0 = do
    let (tok, cur1) = nextToken src cur0
    case tkKind tok of
        TkIf -> parseIf src params cur1 acc0
        _    -> parseRel src params cur0 acc0

-- if cond then thenE else elseE  →  build IIfThenElse with three sub-items.
parseIf :: Source -> [ByteString] -> Cursor -> [Item] -> IO ([Item], Cursor)
parseIf src params cur0 acc0 = do
    (condRev, curC) <- parseExpr src params cur0 []
    let (tThen, curT0) = nextToken src curC
    case tkKind tThen of
        TkThen -> do
            (thenRev, curT) <- parseExpr src params curT0 []
            let (tElse, curE0) = nextToken src curT
            case tkKind tElse of
                TkElse -> do
                    (elseRev, curE) <- parseExpr src params curE0 []
                    let item = IIfThenElse (reverse condRev) (reverse thenRev) (reverse elseRev)
                    pure (item : acc0, curE)
                _ -> throwIO (ParseError ("expected `else` at offset "
                                          <> show (tkStart tElse)
                                          <> " but saw " <> show (tkKind tElse)))
        _ -> throwIO (ParseError ("expected `then` at offset "
                                  <> show (tkStart tThen)
                                  <> " but saw " <> show (tkKind tThen)))

-- relational layer: optional single `<=`.
parseRel :: Source -> [ByteString] -> Cursor -> [Item] -> IO ([Item], Cursor)
parseRel src params cur0 acc0 = do
    (acc1, cur1) <- parseSum src params cur0 acc0
    let (tok, curN) = nextToken src cur1
    case tkKind tok of
        TkLe -> do
            (acc2, cur2) <- parseSum src params curN (IPushX0 : acc1)
            pure (ICmpLe : IPopX1 : acc2, cur2)
        _ -> pure (acc1, cur1)

parseSum :: Source -> [ByteString] -> Cursor -> [Item] -> IO ([Item], Cursor)
parseSum src params cur0 acc0 = do
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
        TkLParen -> do
            (acc1, cur2) <- parseExpr src params cur1 acc0
            let (close, cur3) = nextToken src cur2
            case tkKind close of
                TkRParen -> pure (acc1, cur3)
                _        -> throwIO (ParseError ("expected ')' at offset "
                                                 <> show (tkStart close)
                                                 <> " but saw " <> show (tkKind close)))
        TkIdent name
            | name `elem` params ->
                pure (IArg : acc0, cur1)
            | otherwise -> do
                -- Look for a 1-arg call: `f <atomArg>` where atomArg is
                -- INT, a parameter, or a parenthesized expression.
                let (nextTok, cur2) = nextToken src cur1
                case tkKind nextTok of
                    TkInt k ->
                        pure (ICall1 name : ILitInt (fromInteger k) : acc0, cur2)
                    TkIdent arg
                        | arg `elem` params ->
                            pure (ICall1 name : IArg : acc0, cur2)
                    TkLParen -> do
                        (acc1, cur3) <- parseExpr src params cur2 acc0
                        let (close, cur4) = nextToken src cur3
                        case tkKind close of
                            TkRParen -> pure (ICall1 name : acc1, cur4)
                            _        -> throwIO (ParseError ("expected ')' at offset "
                                                             <> show (tkStart close)
                                                             <> " but saw " <> show (tkKind close)))
                    _ ->
                        pure (ICall name : acc0, cur1)
        TkEof ->
            throwIO (ParseError ("empty expression at offset " <> show (tkStart tok)))
        other ->
            throwIO (ParseError ("unexpected token at offset "
                                 <> show (tkStart tok)
                                 <> ": " <> show other))
