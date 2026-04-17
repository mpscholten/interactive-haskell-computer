-- | Runtime type-family reduction for DataKinds use.
--
-- ihc deliberately has no typechecker — type-level information is
-- discarded except where it escapes back into runtime values through
-- 'GHC.TypeLits.symbolVal', 'GHC.TypeLits.natVal', and friends.  For
-- those call sites we need to reduce a raw type expression like
-- @GetTableName User@ down to its Symbol/Nat literal.  That's what
-- this module does.
--
-- This is NOT a full GHC type-family solver.  It's tuned for the
-- shapes actually observed in IHP's runtime path:
--
--   * Open families with 'type instance' clauses:
--     @type family GetTableName model :: Symbol@ plus
--     @type instance GetTableName User = "users"@
--     →  reduce @GetTableName User@ to @"users"@.
--
--   * Closed families with a @where@ equation list:
--     @type family FieldIndex n xs :: Nat where
--        FieldIndex n (n ': rest) = 0
--        FieldIndex n (m ': rest) = 1 + FieldIndex n rest@
--     →  reduce @FieldIndex "age" '["id","age"]@ to @1@.
--
-- The reducer is conservative: if a pattern doesn't match exactly or a
-- sub-application can't be reduced, we return 'Nothing' and the caller
-- falls back to the existing behaviour (which for 'symbolVal' is the
-- @parseTyArgLit@ best-effort label extractor).
--
-- Design notes
-- ~~~~~~~~~~~~
-- We work on the raw source bytes captured by the parser's
-- 'captureTypeArg', then tokenise them into a small 'TyTok' stream and
-- parse into a recursive 'TyExpr' tree with at most three shapes:
-- literal (string / nat / char), constructor/name, or application.
-- Everything is bytes-in, bytes-out — no hooking into the value
-- evaluator, no Env, no IORefs at the reduction boundary.  That keeps
-- the reducer trivial to thread through the runtime: Scheduler collects
-- a 'TypeFamilyRegistry' once per load and hands the read-only snapshot
-- to the DataKinds builtins via an IORef.
module IHC.TypeReduce
    ( -- * Registry types
      TypeFamilyRegistry
    , emptyRegistry
    , FamilyClause(..)
    , addOpenInstance
    , addClosedEquations
      -- * Reduction
    , reduceTypeExpr
      -- * Global registry (used by the symbolVal / natVal path)
    , globalRegistry
    , setGlobalRegistry
    , getGlobalRegistry
      -- * Exposed for tests
    , TyExpr(..)
    , parseTypeExpr
    , renderTyExpr
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit, isAlpha, isAlphaNum)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Registry
--------------------------------------------------------------------------------

-- | One equation of a type family: pattern arguments on the LHS and the
-- RHS as a type expression.
--
-- For an /open/ @type instance F a b = rhs@ we store one 'FamilyClause'
-- with @['TyExpr' a, 'TyExpr' b]@ as patterns and @rhs@ as the RHS.
--
-- For a /closed/ @type family F where F p1 p2 = r1; F q1 q2 = r2@ we
-- store a list of 'FamilyClause's in declaration order; reduction tries
-- them top-to-bottom and takes the first match.
data FamilyClause = FamilyClause
    { fcPatterns :: ![TyExpr]
    , fcRhs      :: !TyExpr
    } deriving (Eq, Show)

-- | Registry keyed by family (type constructor) name.  The value is the
-- list of clauses in declaration order.  Open instances and closed
-- equations both land here; reduction doesn't distinguish them — the
-- first matching pattern wins in either case.
type TypeFamilyRegistry = Map ByteString [FamilyClause]

emptyRegistry :: TypeFamilyRegistry
emptyRegistry = Map.empty

-- | Register a single open-family instance.  Multiple instances of the
-- same family accumulate; the order is the order of registration.
addOpenInstance :: ByteString -> [TyExpr] -> TyExpr -> TypeFamilyRegistry
                -> TypeFamilyRegistry
addOpenInstance fam pats rhs =
    Map.insertWith (\new old -> old ++ new) fam [FamilyClause pats rhs]

-- | Register all equations of a closed type family at once.  Equations
-- are appended in declaration order.
addClosedEquations :: ByteString -> [FamilyClause] -> TypeFamilyRegistry
                   -> TypeFamilyRegistry
addClosedEquations fam eqs =
    Map.insertWith (\new old -> old ++ new) fam eqs

--------------------------------------------------------------------------------
-- Type expressions
--------------------------------------------------------------------------------

-- | A minimal type expression tree.  Designed to round-trip the shapes
-- that show up in IHP's runtime type-family use; anything we can't
-- represent is kept verbatim as a 'TyRaw' blob so we can still fall
-- back to raw-bytes matching.
data TyExpr
    = TyCon  !ByteString                  -- ^ @User@, @Int@, @Symbol@, @'[]@, etc.
    | TyVar  !ByteString                  -- ^ a lowercase type variable (pattern or RHS)
    | TyLitS !ByteString                  -- ^ Symbol literal @"users"@
    | TyLitN !Integer                     -- ^ Nat literal @42@, @0@, …
    | TyLitC !Char                        -- ^ Char literal @\'x\'@
    | TyApp  !TyExpr !TyExpr              -- ^ @F a@ / @Maybe Int@
    | TyList ![TyExpr]                    -- ^ promoted list @\'[a, b, c]@ and @[T]@
    | TyTup  ![TyExpr]                    -- ^ promoted tuple @\'(a, b)@ and @(a, b)@
    | TyRaw  !ByteString                  -- ^ unreducible fallback
    deriving (Eq, Show)

-- | Render a 'TyExpr' back to source bytes.  Used for diagnostics and
-- for round-tripping a reduced tree to the symbolVal/natVal extractor.
-- The output is syntactically valid Haskell type-expression bytes but
-- may differ from the original whitespace/parenthesisation.
renderTyExpr :: TyExpr -> ByteString
renderTyExpr = BC.pack . go
  where
    go (TyCon n)  = BC.unpack n
    go (TyVar n)  = BC.unpack n
    go (TyLitS s) = '"' : BC.unpack s ++ "\""
    go (TyLitN n) = show n
    go (TyLitC c) = '\'' : c : "'"
    go (TyApp f x) = go f <> " " <> paren x
    go (TyList xs) = "[" <> commaSep xs <> "]"
    go (TyTup  xs) = "(" <> commaSep xs <> ")"
    go (TyRaw s)  = BC.unpack s

    paren e@TyApp{} = "(" <> go e <> ")"
    paren e         = go e

    commaSep = foldr join ""
      where
        join e "" = go e
        join e s  = go e <> ", " <> s

--------------------------------------------------------------------------------
-- Tokeniser
--
-- Not shared with the main lexer: the main lexer is tuned for
-- whole-source scanning with a Source + Cursor abstraction, whereas
-- here we operate on a small ByteString slice that the parser already
-- carved out (see 'captureTypeArg').  Inlining a minimal tokeniser
-- avoids allocating a full Source for each 'reduceTypeExpr' call.
--------------------------------------------------------------------------------

data TyTok
    = TTCon !ByteString
    | TTVar !ByteString
    | TTStr !ByteString
    | TTNat !Integer
    | TTChar !Char
    | TTLParen
    | TTRParen
    | TTLBrack
    | TTRBrack
    | TTComma
    | TTTick        -- ^ @'@ before a promoted constructor or list/tuple
    | TTColonCons   -- ^ @':@ used inside promoted lists
    | TTPlus        -- ^ @+@ on Nat
    | TTMinus
    | TTStar
    deriving (Eq, Show)

tokenise :: ByteString -> [TyTok]
tokenise = go . trim
  where
    trim = BC.dropWhile isSpace . BC.reverse . BC.dropWhile isSpace . BC.reverse
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

    go bs
      | BS.null bs = []
      | otherwise = case BC.head bs of
          '(' -> TTLParen : go (BC.tail bs)
          ')' -> TTRParen : go (BC.tail bs)
          '[' -> TTLBrack : go (BC.tail bs)
          ']' -> TTRBrack : go (BC.tail bs)
          ',' -> TTComma  : go (BC.tail bs)
          '+' -> TTPlus   : go (BC.tail bs)
          '*' -> TTStar   : go (BC.tail bs)
          '-' -> TTMinus  : go (BC.tail bs)
          '\'' ->
              let r1 = BC.tail bs
              in case BC.uncons r1 of
                  -- ': as a symbol (promoted cons) — note we require ':' right after '
                  Just (':', rest) -> TTColonCons : go rest
                  -- '[ or '( — the tick introduces a promoted list/tuple
                  Just ('[', _)    -> TTTick : go r1
                  Just ('(', _)    -> TTTick : go r1
                  Just _           ->
                      -- Either a char literal 'x' or a promoted constructor 'Con
                      case parseCharOrPromoted r1 of
                          Just (tok, rest') -> tok : go rest'
                          Nothing           -> go r1
                  Nothing          -> []
          '"' ->
              let (lit, rest) = parseStr (BC.tail bs)
              in TTStr lit : go rest
          c | c == ' ' || c == '\t' || c == '\n' || c == '\r' -> go (BC.tail bs)
            | isDigit c ->
              case BC.readInteger bs of
                  Just (n, rest) -> TTNat n : go rest
                  Nothing        -> go (BC.tail bs)
            | isAlpha c || c == '_' ->
              let (ident, rest) = BC.span isIdentChar bs
              in (classifyIdent ident) : go rest
            | otherwise -> go (BC.tail bs)  -- skip unknowns

    isIdentChar c = isAlphaNum c || c == '_' || c == '\'' || c == '.'

    classifyIdent bs =
        case BC.uncons bs of
            Just (c, _)
              | isAlpha c && c >= 'A' && c <= 'Z' -> TTCon bs
              | otherwise                         -> TTVar bs
            Nothing -> TTVar bs

    parseStr bs =
        let (body, rest) = BC.break (== '"') bs
        in (body, if BS.null rest then rest else BC.tail rest)

    parseCharOrPromoted bs = case BC.uncons bs of
        Just (c, rest)
            -- 'Foo — promoted constructor.  Lexer already grabbed the tick
            -- separately; the rest is the identifier.
            | isAlpha c && c >= 'A' && c <= 'Z' ->
                let (ident, rest') = BC.span isIdentChar bs
                in Just (TTCon ident, rest')
            -- 'x' char literal — the second char, then a closing '\''
            | otherwise ->
                case BC.uncons rest of
                    Just ('\'', rest') -> Just (TTChar c, rest')
                    _                  -> Nothing
        Nothing -> Nothing

--------------------------------------------------------------------------------
-- Parser (tokens → TyExpr)
--
-- Grammar:
--
-- @
-- type   ::= app ('+' app)*                  -- Nat arithmetic
-- app    ::= atom atom*
-- atom   ::= '"…"'                           -- Symbol literal
--          | Nat | Char
--          | Con | var
--          | ''[' type (',' type)* ']'       -- promoted list
--          | '[' (type (',' type)*)? ']'     -- list / promoted-list
--          | '('  … ')'                       -- paren or tuple
--          | ''('   … ')'                     -- promoted tuple
-- @
--
-- Comma-separated items inside brackets or tuples are each a full type.
-- We also accept bare @':@ as promoted cons for things like @(x ': xs)@.
--------------------------------------------------------------------------------

-- | Parse a raw type-expression slice into a 'TyExpr'.  Bytes that
-- don't fit the grammar fall through to 'TyRaw'.
parseTypeExpr :: ByteString -> TyExpr
parseTypeExpr bs =
    let toks = tokenise bs
    in case parseType toks of
        Just (e, []) -> e
        Just (e, _)  -> e   -- tolerate trailing junk
        Nothing      -> TyRaw bs

type Parser a = [TyTok] -> Maybe (a, [TyTok])

parseType :: Parser TyExpr
parseType toks = do
    (first, rest) <- parseApp toks
    parseAddTail first rest

-- | After an atom/app, handle @+@ chains: @a + b + c@.  On Nat.
parseAddTail :: TyExpr -> Parser TyExpr
parseAddTail lhs (TTPlus : rest) = do
    (rhs, rest') <- parseApp rest
    let combined = TyApp (TyApp (TyCon (BC.pack "+")) lhs) rhs
    parseAddTail combined rest'
parseAddTail lhs rest = Just (lhs, rest)

-- | One or more atoms juxtaposed (@F a b@).  Returns the left-associated
-- application tree.
parseApp :: Parser TyExpr
parseApp toks = do
    (hd, rest) <- parseAtom toks
    loop hd rest
  where
    loop acc rest0 =
        case parseAtom rest0 of
            Just (a, rest1) -> loop (TyApp acc a) rest1
            Nothing         -> Just (acc, rest0)

parseAtom :: Parser TyExpr
parseAtom (TTStr s      : rest) = Just (TyLitS s,  rest)
parseAtom (TTNat n      : rest) = Just (TyLitN n,  rest)
parseAtom (TTChar c     : rest) = Just (TyLitC c,  rest)
parseAtom (TTCon c      : rest) = Just (TyCon  c,  rest)
parseAtom (TTVar v      : rest) = Just (TyVar  v,  rest)
-- Promoted list or tuple: ' introduces [...] or (...).  The tick itself
-- has no semantic content for reduction; we treat '[ ... ] the same as
-- [ ... ] here (both produce 'TyList').
parseAtom (TTTick : TTLBrack : rest) = parseListBody rest
parseAtom (TTTick : TTLParen : rest) = parseTupleBody rest
parseAtom (TTLBrack : rest)          = parseListBody rest
parseAtom (TTLParen : rest) =
    case parseType rest of
        -- () unit or (,) tuple — treat as paren grouping first.
        Just (e, TTRParen : rest') -> Just (e, rest')
        Just (first, TTComma : rest') ->
            case parseTupleRest [first] rest' of
                Just (es, rest'') -> Just (TyTup es, rest'')
                Nothing           -> Nothing
        -- (x ': xs) — cons pattern as a paren-wrapped atom. Produce
        -- 'TyApp (TyApp (TyCon "':") lhs) rhs'; 'match' handles
        -- destructuring this against a concrete 'TyList'.
        Just (lhs, TTColonCons : rest') ->
            case parseType rest' of
                Just (rhs, TTRParen : rest'') ->
                    Just (TyApp (TyApp (TyCon (BC.pack "':")) lhs) rhs, rest'')
                _ -> Nothing
        _ -> Nothing
parseAtom _ = Nothing

-- | Parse the body of a list @... ]@ — caller has already consumed the
-- opening @[@.  Handles @':@ used as a cons operator on promoted lists
-- by producing a regular TyList from the head/tail structure.
parseListBody :: Parser TyExpr
parseListBody (TTRBrack : rest) = Just (TyList [], rest)
parseListBody toks = case parseType toks of
    Just (first, TTRBrack : rest) -> Just (TyList [first], rest)
    Just (first, TTComma : rest) ->
        case parseListRest [first] rest of
            Just (es, rest') -> Just (TyList es, rest')
            Nothing          -> Nothing
    Just (first, TTColonCons : rest) ->
        -- [x ': xs]  — rare surface, but honour it as cons.
        case parseType rest of
            Just (rest', TTRBrack : rest2) ->
                Just (TyApp (TyApp (TyCon (BC.pack "':")) first) rest', rest2)
            _ -> Nothing
    _ -> Nothing

parseListRest :: [TyExpr] -> Parser [TyExpr]
parseListRest acc toks = case parseType toks of
    Just (e, TTRBrack : rest) -> Just (reverse (e : acc), rest)
    Just (e, TTComma  : rest) -> parseListRest (e : acc) rest
    _                          -> Nothing

parseTupleBody :: Parser TyExpr
parseTupleBody toks = case parseType toks of
    Just (first, TTRParen : rest) -> Just (TyTup [first], rest)
    Just (first, TTComma  : rest) ->
        case parseTupleRest [first] rest of
            Just (es, rest') -> Just (TyTup es, rest')
            Nothing          -> Nothing
    _ -> Nothing

parseTupleRest :: [TyExpr] -> Parser [TyExpr]
parseTupleRest acc toks = case parseType toks of
    Just (e, TTRParen : rest) -> Just (reverse (e : acc), rest)
    Just (e, TTComma  : rest) -> parseTupleRest (e : acc) rest
    _                          -> Nothing

--------------------------------------------------------------------------------
-- Reduction
--------------------------------------------------------------------------------

-- | Try to reduce a raw type-expression slice to a terminal shape
-- (Symbol literal, Nat literal, or concrete constructor).  Returns the
-- reduced bytes on success.
--
-- On failure (no matching clause, unknown head, unsupported shape) we
-- return 'Nothing' and the caller keeps the original bytes.
reduceTypeExpr :: TypeFamilyRegistry -> ByteString -> Maybe ByteString
reduceTypeExpr reg bs =
    let initial = parseTypeExpr bs
        reduced = reduce reg initial
    in if reduced == initial
         then Nothing
         else Just (renderTyExpr reduced)

-- | Normal-order reduction: repeatedly try the outermost family
-- application.  Stops when no rewrite is possible.  A fuel bound
-- protects against pathological recursion (shouldn't happen with
-- IHP's well-founded shapes but cheap insurance).
reduce :: TypeFamilyRegistry -> TyExpr -> TyExpr
reduce reg = go (100 :: Int)
  where
    go 0 e = e
    go fuel e = case step reg e of
        Just e' | e' /= e -> go (fuel - 1) e'
        _                 -> e

-- | One reduction step.  Tries, in order:
--
--   * Nat arithmetic on the outer expression (@N + 0@, @0 + N@, @a + b@
--     where both sides are 'TyLitN').
--   * Family application at the head — match the spine against the
--     registered clauses.
--   * Recurse into sub-expressions.
step :: TypeFamilyRegistry -> TyExpr -> Maybe TyExpr
step _   e@(TyLitS _) = Just e
step _   e@(TyLitN _) = Just e
step _   e@(TyLitC _) = Just e
step _   e@(TyCon _)  = Just e
step _   e@(TyVar _)  = Just e
step reg (TyApp (TyApp (TyCon plus) a) b)
  | plus == BC.pack "+" =
      let a' = reduce reg a
          b' = reduce reg b
      in case (a', b') of
          (TyLitN x, TyLitN y) -> Just (TyLitN (x + y))
          (TyLitN 0, _)         -> Just b'
          (_,         TyLitN 0) -> Just a'
          _                     -> Just (TyApp (TyApp (TyCon plus) a') b')
step reg e@(TyApp _ _) =
    case reduceHead reg e of
        Just e' -> Just e'
        Nothing ->
            -- Can't reduce the head — try to reduce sub-expressions so
            -- that e.g. @F (G a) b@ becomes @F result b@ and a later
            -- outer step can match.
            let (hd, args) = spine e
                args'       = map (reduce reg) args
                e'          = foldl TyApp hd args'
            in if e' /= e then Just e' else Just e
step reg (TyList xs) = Just (TyList (map (reduce reg) xs))
step reg (TyTup  xs) = Just (TyTup  (map (reduce reg) xs))
step _   e           = Just e

-- | Try to pattern-match @F args@ against any clause registered for
-- family @F@.  Returns the RHS with pattern variables substituted on a
-- match, or 'Nothing' if no clause applies.
reduceHead :: TypeFamilyRegistry -> TyExpr -> Maybe TyExpr
reduceHead reg expr =
    let (hd, args) = spine expr
    in case hd of
        TyCon fam ->
            case Map.lookup fam reg of
                Just clauses -> tryClauses reg clauses args
                Nothing      -> Nothing
        _ -> Nothing

tryClauses :: TypeFamilyRegistry -> [FamilyClause] -> [TyExpr] -> Maybe TyExpr
tryClauses _   []                               _    = Nothing
tryClauses reg (FamilyClause pats rhs : rest) args
    | length pats /= length args = tryClauses reg rest args
    | otherwise = case matchAll Map.empty pats args of
          Just env -> Just (substitute env rhs)
          Nothing  -> tryClauses reg rest args

-- | Split @F a b c@ into @(F, [a, b, c])@.
spine :: TyExpr -> (TyExpr, [TyExpr])
spine = go []
  where
    go acc (TyApp f x) = go (x : acc) f
    go acc e           = (e, acc)

--------------------------------------------------------------------------------
-- Pattern matching
--------------------------------------------------------------------------------

type Subst = Map ByteString TyExpr

matchAll :: Subst -> [TyExpr] -> [TyExpr] -> Maybe Subst
matchAll env [] [] = Just env
matchAll env (p:ps) (a:as) = do
    env' <- match env p a
    matchAll env' ps as
matchAll _ _ _ = Nothing

-- | Match one pattern against one argument, extending the substitution
-- on success.  Matching is structural; a pattern variable @v@ binds to
-- whatever sits in its slot, with consistency checked if @v@ appears
-- more than once (non-linear pattern, used by FieldIndex's @n (n ':
-- rest)@ form).
match :: Subst -> TyExpr -> TyExpr -> Maybe Subst
match env (TyVar v) a =
    case Map.lookup v env of
        Just bound
          | bound == a -> Just env
          | otherwise  -> Nothing
        Nothing        -> Just (Map.insert v a env)
match env (TyCon c) (TyCon c')
    | c == c'   = Just env
    | otherwise = Nothing
match env (TyLitS s) (TyLitS s')
    | s == s'   = Just env
    | otherwise = Nothing
match env (TyLitN n) (TyLitN n')
    | n == n'   = Just env
    | otherwise = Nothing
match env (TyLitC c) (TyLitC c')
    | c == c'   = Just env
    | otherwise = Nothing
match env (TyApp p1 p2) (TyApp a1 a2) = do
    env' <- match env p1 a1
    match env' p2 a2
match env (TyList ps) (TyList as)
    | length ps == length as = matchAll env ps as
    | otherwise              = Nothing
-- Cons pattern '(x ': xs)' against a concrete promoted list: split head/tail.
match env (TyApp (TyApp (TyCon c) ph) pt) (TyList (a:as))
    | c == BC.pack "':" = do
        env' <- match env ph a
        match env' pt (TyList as)
-- Same shape against a cons-shaped concrete value (e.g. recursive cases
-- that haven't re-packed into TyList).
match env (TyApp (TyApp (TyCon c) ph) pt) (TyApp (TyApp (TyCon c') ah) at)
    | c == BC.pack "':" && c' == BC.pack "':" = do
        env' <- match env ph ah
        match env' pt at
match env (TyTup ps) (TyTup as)
    | length ps == length as = matchAll env ps as
    | otherwise              = Nothing
match _ _ _ = Nothing

-- | Apply a substitution to an RHS.
substitute :: Subst -> TyExpr -> TyExpr
substitute env = go
  where
    go (TyVar v)
        | Just e <- Map.lookup v env = e
    go (TyApp a b) = TyApp (go a) (go b)
    go (TyList xs) = TyList (map go xs)
    go (TyTup  xs) = TyTup  (map go xs)
    go other       = other

--------------------------------------------------------------------------------
-- Global registry
--
-- We stash the TF registry in an IORef so the symbolVal / natVal
-- builtins (which are plain @IO Val@ functions with no Env parameter)
-- can reach it.  Scheduler installs the fully-merged registry once per
-- program load; REPL sessions overwrite with every :l.
--
-- Using 'unsafePerformIO' here follows the same pattern as
-- IHC.Classes's ClassRegistry-hook — see 'Classes.hs'.
--------------------------------------------------------------------------------

{-# NOINLINE globalRegistry #-}
globalRegistry :: IORef TypeFamilyRegistry
globalRegistry = unsafePerformIO (newIORef Map.empty)

setGlobalRegistry :: TypeFamilyRegistry -> IO ()
setGlobalRegistry = writeIORef globalRegistry

getGlobalRegistry :: IO TypeFamilyRegistry
getGlobalRegistry = readIORef globalRegistry

