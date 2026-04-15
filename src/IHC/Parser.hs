{-# LANGUAGE DeriveAnyClass #-}

-- | Single-pass recursive-descent parser for binding bodies.
--
-- Parses the body once, producing a small 'Item' list. No AST.
--
-- Grammar (Phase 1.11):
--
-- @
-- body    ::= expr
-- expr    ::= 'if' expr 'then' expr 'else' expr
--           | 'do' do-block
--           | 'let' ident '=' expr 'in' expr
--           | or
-- or      ::= and  ('||' and)*
-- and     ::= rel  ('&&' rel)*
-- rel     ::= sum  (relop sum)?           -- at most one comparison
-- sum     ::= term (('+' | '-' | '++') term)*
-- term    ::= app  ('*' app)*
-- app     ::= ident atom*                 -- function application
--           | atom
-- atom    ::= INT | STRING
--           | '(' expr ')'
--           | '-' atom                    -- unary minus
--           | ident                       -- param, local-let, or nullary call
-- @
module IHC.Parser
    ( parseBodyItems
    , ParseError(..)
    , ArityResolver
    ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.List (elemIndex)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import IHC.Encode (Cond(..))
import IHC.IR
import IHC.Lexer
import IHC.Source

newtype ParseError = ParseError String
    deriving stock (Show)
    deriving anyclass (Exception)

type ArityResolver = ByteString -> IO Int

-- | Parser context — bundled to avoid threading three or four args
-- through every recursive call.
data Ctx = Ctx
    { ctxSrc     :: !Source
    , ctxParams  :: ![ByteString]              -- enclosing function's params
    , ctxLocals  :: !(Map ByteString [Item])   -- let-bound names -> items
    , ctxResolve :: !ArityResolver
    }

-- | Parse a binding body's span given its param list and a resolver
-- that knows every referenced top-level binding's arity.
parseBodyItems :: Source -> [ByteString] -> ArityResolver -> Span -> IO [Item]
parseBodyItems src params resolve (start, _end) = do
    let cur0 = Cursor start 1 1
        ctx  = Ctx src params Map.empty resolve
    (items, _cur1) <- parseExpr ctx cur0 []
    pure (reverse items)

-- | 'nextToken' variant that skips 'TkNewline', letting an expression
-- flow across indented continuation lines.
nextSig :: Source -> Cursor -> (Token, Cursor)
nextSig src cur =
    let (t, c) = nextToken src cur in
    case tkKind t of
        TkNewline -> nextSig src c
        _         -> (t, c)

parseExpr :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseExpr ctx cur0 acc0 = do
    let (tok, cur1) = nextSig (ctxSrc ctx) cur0
    case tkKind tok of
        TkIf  -> parseIf  ctx cur1 acc0
        TkDo  -> parseDo  ctx cur1 acc0
        TkLet -> parseLet ctx cur1 acc0
        _     -> parseOr  ctx cur0 acc0

-- @let ident = e1 in e2@
--
-- Captures e1's items in the locals map; e2 is parsed with that
-- extension. Each occurrence of the let-bound name in e2 splices a
-- fresh copy of e1's items at the call site.
--
-- Caveat: this is splice/inline semantics. For pure expressions it
-- matches Haskell's call-by-name; for IO actions with shared effects
-- it would re-evaluate, which we don't try to support here.
parseLet :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseLet ctx cur0 acc0 = do
    let (nameTok, cur1) = nextSig (ctxSrc ctx) cur0
    name <- case tkKind nameTok of
        TkIdent n -> pure n
        _         -> parseErr "expected identifier after `let`" nameTok
    let (eqTok, cur2) = nextSig (ctxSrc ctx) cur1
    case tkKind eqTok of
        TkEq -> pure ()
        _    -> parseErr "expected `=` in let-binding" eqTok
    -- Parse e1 into its own item list (reversed during build).
    (e1Rev, cur3) <- parseExpr ctx cur2 []
    let e1Items = reverse e1Rev
    -- Expect `in`.
    let (inTok, cur4) = nextSig (ctxSrc ctx) cur3
    case tkKind inTok of
        TkIn -> pure ()
        _    -> parseErr "expected `in` in let-binding" inTok
    -- Parse e2 with the local binding in scope.
    let ctx' = ctx { ctxLocals = Map.insert name e1Items (ctxLocals ctx) }
    parseExpr ctx' cur4 acc0

-- @do { stmt ; ... ; stmt }@ (explicit braces) or layout form.
parseDo :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseDo ctx cur0 acc0 = do
    let (firstTok, curAfter) = nextSig (ctxSrc ctx) cur0
    case tkKind firstTok of
        TkLBrace -> bracedStmts curAfter acc0
        TkEof    -> pure (acc0, cur0)
        _        -> layoutStmts (tkCol firstTok) cur0 acc0
  where
    bracedStmts cur acc = do
        (acc', cur')  <- parseExpr ctx cur acc
        let (sep, curN) = nextSig (ctxSrc ctx) cur'
        case tkKind sep of
            TkSemi   -> bracedStmts curN acc'
            TkRBrace -> pure (acc', curN)
            _        -> parseErr "expected `;` or `}` in do-block" sep

    layoutStmts stmtCol cur acc = do
        (acc', cur') <- parseExpr ctx cur acc
        let (nextTok, _) = nextSig (ctxSrc ctx) cur'
        case tkKind nextTok of
            TkEof -> pure (acc', cur')
            _ | tkCol nextTok == stmtCol -> layoutStmts stmtCol cur' acc'
              | otherwise                -> pure (acc', cur')

parseIf :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseIf ctx cur0 acc0 = do
    (condRev, curC) <- parseExpr ctx cur0 []
    let (tThen, curT0) = nextSig (ctxSrc ctx) curC
    case tkKind tThen of
        TkThen -> do
            (thenRev, curT) <- parseExpr ctx curT0 []
            let (tElse, curE0) = nextSig (ctxSrc ctx) curT
            case tkKind tElse of
                TkElse -> do
                    (elseRev, curE) <- parseExpr ctx curE0 []
                    let item = IIfThenElse (reverse condRev) (reverse thenRev) (reverse elseRev)
                    pure (item : acc0, curE)
                _ -> parseErr "expected `else`" tElse
        _ -> parseErr "expected `then`" tThen

parseOr :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseOr ctx cur0 acc0 = do
    (acc1, cur1) <- parseAnd ctx cur0 acc0
    loop acc1 cur1
  where
    loop acc cur =
        let (tok, curN) = nextSig (ctxSrc ctx) cur in
        case tkKind tok of
            TkOr -> do
                (acc', cur') <- parseAnd ctx curN (IPushX0 : acc)
                loop (IOrX1X0 : IPopX1 : acc') cur'
            _ -> pure (acc, cur)

parseAnd :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseAnd ctx cur0 acc0 = do
    (acc1, cur1) <- parseRel ctx cur0 acc0
    loop acc1 cur1
  where
    loop acc cur =
        let (tok, curN) = nextSig (ctxSrc ctx) cur in
        case tkKind tok of
            TkAnd -> do
                (acc', cur') <- parseRel ctx curN (IPushX0 : acc)
                loop (IAndX1X0 : IPopX1 : acc') cur'
            _ -> pure (acc, cur)

parseRel :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseRel ctx cur0 acc0 = do
    (acc1, cur1) <- parseSum ctx cur0 acc0
    let (tok, curN) = nextSig (ctxSrc ctx) cur1
    case tokToCond (tkKind tok) of
        Just c -> do
            (acc2, cur2) <- parseSum ctx curN (IPushX0 : acc1)
            pure (ICmp c : IPopX1 : acc2, cur2)
        Nothing -> pure (acc1, cur1)
  where
    tokToCond TkLe   = Just CLe
    tokToCond TkLt   = Just CLt
    tokToCond TkGe   = Just CGe
    tokToCond TkGt   = Just CGt
    tokToCond TkEqEq = Just CEq
    tokToCond TkNeq  = Just CNe
    tokToCond _      = Nothing

parseSum :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseSum ctx cur0 acc0 = do
    (acc1, cur1) <- parseTerm ctx cur0 acc0
    loop acc1 cur1
  where
    loop acc cur =
        let (tok, curN) = nextSig (ctxSrc ctx) cur in
        case tkKind tok of
            TkPlus -> do
                (acc', cur') <- parseTerm ctx curN (IPushX0 : acc)
                loop (IAddX1X0 : IPopX1 : acc') cur'
            TkMinus -> do
                (acc', cur') <- parseTerm ctx curN (IPushX0 : acc)
                loop (ISubX1X0 : IPopX1 : acc') cur'
            TkPlusPlus -> do
                (acc', cur') <- parseTerm ctx curN (IPushX0 : acc)
                loop (ICall "##concat" 2 : IPushX0 : acc') cur'
            _ -> pure (acc, cur)

parseTerm :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseTerm ctx cur0 acc0 = do
    (acc1, cur1) <- parseApp ctx cur0 acc0
    loop acc1 cur1
  where
    loop acc cur =
        let (tok, curN) = nextSig (ctxSrc ctx) cur in
        case tkKind tok of
            TkStar -> do
                (acc', cur') <- parseApp ctx curN (IPushX0 : acc)
                loop (IMulX1X0 : IPopX1 : acc') cur'
            _ -> pure (acc, cur)

parseApp :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseApp ctx cur0 acc0 = do
    let (tok, cur1) = nextSig (ctxSrc ctx) cur0
    case tkKind tok of
        TkIdent name
            -- Local let-binding: splice its items.
            | Just localItems <- Map.lookup name (ctxLocals ctx) ->
                pure (reverse localItems ++ acc0, cur1)
            | Just idx <- elemIndex name (ctxParams ctx) ->
                pure (IArg idx : acc0, cur1)
            | otherwise -> do
                arity <- ctxResolve ctx name
                (accAfterArgs, curAfterArgs) <- parseNArgs arity ctx cur1 acc0
                pure (ICall name arity : accAfterArgs, curAfterArgs)
        _ -> parseAtom ctx cur0 acc0

parseNArgs :: Int -> Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseNArgs 0 _ cur acc = pure (acc, cur)
parseNArgs n ctx cur acc = do
    (acc', cur') <- parseAtom ctx cur acc
    parseNArgs (n - 1) ctx cur' (IPushX0 : acc')

parseAtom :: Ctx -> Cursor -> [Item] -> IO ([Item], Cursor)
parseAtom ctx cur0 acc0 = do
    let (tok, cur1) = nextSig (ctxSrc ctx) cur0
    case tkKind tok of
        TkInt n ->
            pure (ILitInt (fromInteger n) : acc0, cur1)
        TkStr s ->
            pure (ILitStr s : acc0, cur1)
        TkMinus -> do
            (acc1, cur2) <- parseAtom ctx cur1 acc0
            pure (INegX0 : acc1, cur2)
        TkLParen -> do
            (acc1, cur2) <- parseExpr ctx cur1 acc0
            let (close, cur3) = nextSig (ctxSrc ctx) cur2
            case tkKind close of
                TkRParen -> pure (acc1, cur3)
                _        -> parseErr "expected ')'" close
        TkIdent name
            | Just localItems <- Map.lookup name (ctxLocals ctx) ->
                pure (reverse localItems ++ acc0, cur1)
            | Just idx <- elemIndex name (ctxParams ctx) ->
                pure (IArg idx : acc0, cur1)
            | otherwise -> do
                arity <- ctxResolve ctx name
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
