{-# LANGUAGE DeriveAnyClass #-}

-- | Single-pass recursive-descent parser for binding bodies.
--
-- Parses the body once, producing a small 'Item' list. No AST.
--
-- Grammar (Phase 1.5):
--
-- @
-- body    ::= expr
-- expr    ::= 'if' expr 'then' expr 'else' expr
--           | rel
-- rel     ::= sum  ('<=' sum)?
-- sum     ::= term (('+' | '-') term)*
-- term    ::= app  ('*' app)*
-- app     ::= atom atom*                   -- function application (exactly
--                                          -- @arity@ atoms consumed per the
--                                          -- callee's LHS)
-- atom    ::= INT
--           | '(' expr ')'
--           | ident
-- @
--
-- The parser receives two callbacks:
--   * the enclosing binding's parameter list (so an ident matching a
--     param becomes 'IArg'),
--   * an 'ArityResolver' returning the arity of any referenced
--     top-level binding (so we know how many atoms to eat after the
--     call's ident).
module IHC.Parser
    ( parseBodyItems
    , ParseError(..)
    , ArityResolver
    ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.List (elemIndex)
import Data.Maybe (fromJust)

import IHC.IR
import IHC.Lexer
import IHC.Source

newtype ParseError = ParseError String
    deriving stock (Show)
    deriving anyclass (Exception)

type ArityResolver = ByteString -> IO Int

-- | Parse a binding body's span given its param list and a resolver
-- that knows every referenced top-level binding's arity.
parseBodyItems :: Source -> [ByteString] -> ArityResolver -> Span -> IO [Item]
parseBodyItems src params resolve (start, _end) = do
    let cur0 = Cursor start 1 1
    (items, _cur1) <- parseExpr src params resolve cur0 []
    pure (reverse items)

-- | 'nextToken' variant that skips 'TkNewline', letting an expression
-- flow across indented continuation lines.
nextSig :: Source -> Cursor -> (Token, Cursor)
nextSig src cur =
    let (t, c) = nextToken src cur in
    case tkKind t of
        TkNewline -> nextSig src c
        _         -> (t, c)

parseExpr :: Source -> [ByteString] -> ArityResolver -> Cursor -> [Item] -> IO ([Item], Cursor)
parseExpr src params resolve cur0 acc0 = do
    let (tok, cur1) = nextSig src cur0
    case tkKind tok of
        TkIf -> parseIf src params resolve cur1 acc0
        _    -> parseRel src params resolve cur0 acc0

parseIf :: Source -> [ByteString] -> ArityResolver -> Cursor -> [Item] -> IO ([Item], Cursor)
parseIf src params resolve cur0 acc0 = do
    (condRev, curC) <- parseExpr src params resolve cur0 []
    let (tThen, curT0) = nextSig src curC
    case tkKind tThen of
        TkThen -> do
            (thenRev, curT) <- parseExpr src params resolve curT0 []
            let (tElse, curE0) = nextSig src curT
            case tkKind tElse of
                TkElse -> do
                    (elseRev, curE) <- parseExpr src params resolve curE0 []
                    let item = IIfThenElse (reverse condRev) (reverse thenRev) (reverse elseRev)
                    pure (item : acc0, curE)
                _ -> parseErr "expected `else`" tElse
        _ -> parseErr "expected `then`" tThen

parseRel :: Source -> [ByteString] -> ArityResolver -> Cursor -> [Item] -> IO ([Item], Cursor)
parseRel src params resolve cur0 acc0 = do
    (acc1, cur1) <- parseSum src params resolve cur0 acc0
    let (tok, curN) = nextSig src cur1
    case tkKind tok of
        TkLe -> do
            (acc2, cur2) <- parseSum src params resolve curN (IPushX0 : acc1)
            pure (ICmpLe : IPopX1 : acc2, cur2)
        _ -> pure (acc1, cur1)

parseSum :: Source -> [ByteString] -> ArityResolver -> Cursor -> [Item] -> IO ([Item], Cursor)
parseSum src params resolve cur0 acc0 = do
    (acc1, cur1) <- parseTerm src params resolve cur0 acc0
    loop acc1 cur1
  where
    loop acc cur =
        let (tok, curN) = nextSig src cur in
        case tkKind tok of
            TkPlus -> do
                (acc', cur') <- parseTerm src params resolve curN (IPushX0 : acc)
                loop (IAddX1X0 : IPopX1 : acc') cur'
            TkMinus -> do
                (acc', cur') <- parseTerm src params resolve curN (IPushX0 : acc)
                loop (ISubX1X0 : IPopX1 : acc') cur'
            _ -> pure (acc, cur)

parseTerm :: Source -> [ByteString] -> ArityResolver -> Cursor -> [Item] -> IO ([Item], Cursor)
parseTerm src params resolve cur0 acc0 = do
    (acc1, cur1) <- parseApp src params resolve cur0 acc0
    loop acc1 cur1
  where
    loop acc cur =
        let (tok, curN) = nextSig src cur in
        case tkKind tok of
            TkStar -> do
                (acc', cur') <- parseApp src params resolve curN (IPushX0 : acc)
                loop (IMulX1X0 : IPopX1 : acc') cur'
            _ -> pure (acc, cur)

-- | Function application. An identifier (that isn't a parameter) is
-- followed by exactly @arity@ atoms; each is parsed and pushed, then
-- ICall pops them into x0..x(arity-1) before branching.
parseApp :: Source -> [ByteString] -> ArityResolver -> Cursor -> [Item] -> IO ([Item], Cursor)
parseApp src params resolve cur0 acc0 = do
    let (tok, cur1) = nextSig src cur0
    case tkKind tok of
        TkIdent name
            | Just idx <- elemIndex name params ->
                pure (IArg idx : acc0, cur1)
            | otherwise -> do
                arity <- resolve name
                (accAfterArgs, curAfterArgs) <- parseNArgs arity src params resolve cur1 acc0
                pure (ICall name arity : accAfterArgs, curAfterArgs)
        _ -> parseAtom src params resolve cur0 acc0

parseNArgs :: Int -> Source -> [ByteString] -> ArityResolver -> Cursor -> [Item] -> IO ([Item], Cursor)
parseNArgs 0 _ _ _ cur acc = pure (acc, cur)
parseNArgs n src params resolve cur acc = do
    (acc', cur') <- parseAtom src params resolve cur acc
    -- The arg's value is in x0; push it before moving to the next arg.
    parseNArgs (n - 1) src params resolve cur' (IPushX0 : acc')

parseAtom :: Source -> [ByteString] -> ArityResolver -> Cursor -> [Item] -> IO ([Item], Cursor)
parseAtom src params resolve cur0 acc0 = do
    let (tok, cur1) = nextSig src cur0
    case tkKind tok of
        TkInt n ->
            pure (ILitInt (fromInteger n) : acc0, cur1)
        TkLParen -> do
            (acc1, cur2) <- parseExpr src params resolve cur1 acc0
            let (close, cur3) = nextSig src cur2
            case tkKind close of
                TkRParen -> pure (acc1, cur3)
                _        -> parseErr "expected ')'" close
        TkIdent name
            | Just idx <- elemIndex name params ->
                pure (IArg idx : acc0, cur1)
            | otherwise -> do
                -- As an atom (i.e. in an argument position), a bare
                -- ident is always a *nullary* call — if it had args,
                -- the outer `parseApp` would have grabbed it. Ask the
                -- resolver to confirm arity 0; any positive arity here
                -- means a call site wasn't wrapped in parens.
                arity <- resolve name
                if arity == 0
                    then pure (ICall name 0 : acc0, cur1)
                    else throwIO (ParseError
                        ("`" <> BC.unpack name <> "` expects "
                         <> show arity
                         <> " argument(s) but appears as a bare atom at offset "
                         <> show (tkStart tok)
                         <> " — wrap the call in parentheses"))
        TkEof ->
            throwIO (ParseError ("empty expression at offset " <> show (tkStart tok)))
        _ ->
            parseErr "unexpected token" tok

parseErr :: String -> Token -> IO a
parseErr msg tok =
    throwIO (ParseError (msg <> " at offset " <> show (tkStart tok)
                         <> " but saw " <> show (tkKind tok)))
