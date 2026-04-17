-- | Phase 2.11 — Template Haskell Lift-splice subset.
--
-- This module provides:
--
-- 1. Builtin 'Lift' instances for primitive types (Int, Char, String,
--    Bool, (), lists, tuples, Maybe, Either).  Each instance is a
--    'VFun' that maps a runtime 'Val' to a 'Val' encoding a TH 'Exp'
--    AST node (e.g. @VCon "LitE" [integerLVal]@).
--
-- 2. A synthetic @Language.Haskell.TH.Syntax@ / @Language.Haskell.TH@
--    module surface: 'lift' is the only exported function. The 'Q' monad
--    is not needed for Lift-only splices — we short-circuit it entirely.
--
-- 3. @thExpToExpr :: Val -> IO Expr@ — the anti-quoter that walks a
--    TH 'Exp' value-tree and produces an 'IHC.AST.Expr'.
--
-- 4. @expandSplicesInExpr :: Env -> Int -> Expr -> IO Expr@ — recursively
--    replaces every 'ESplice' node with the 'Expr' it evaluates to.
--    Hard-caps recursion at depth 16.
--
-- NOT in scope: [| |] quotation, reify, Q IO, declaration/type splices.
module IHC.TH
    ( liftBuiltin
    , thExpToExpr
    , expandSplicesInExpr
    , thBuiltinPairs
    , exprToVal
    ) where

import Control.Exception (throwIO, Exception)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Int (Int64)
import IHC.AST
import IHC.Eval (eval, force)
import IHC.Val

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

newtype THError = THError String deriving (Show)
instance Exception THError

throwTH :: String -> IO a
throwTH msg = throwIO (THError ("IHC.TH: " <> msg))

--------------------------------------------------------------------------------
-- TH Exp ADT encoding
--
-- We represent the GHC TH Exp type as VCon nodes, mirroring the real TH
-- constructors but without importing the actual TH library.
--
-- Supported constructors (enough for Lift-only use):
--   LitE Lit         -> VCon "LitE" [litVal]
--   VarE Name        -> VCon "VarE" [thNameVal]
--   ConE Name        -> VCon "ConE" [thNameVal]
--   AppE Exp Exp     -> VCon "AppE" [expThunk, expThunk]
--   TupE [Exp]       -> VCon "TupE" [listOfExps]
--   ListE [Exp]      -> VCon "ListE" [listOfExps]
--
-- Lit constructors:
--   IntegerL Integer -> VCon "IntegerL" [intVal]
--   CharL Char       -> VCon "CharL"    [charVal]
--   StringL String   -> VCon "StringL"  [stringVal]  -- [Char] list
--
-- Name: just stored as VStr (the name bytes).
--------------------------------------------------------------------------------

-- | Build a TH Name value from a ByteString (name string).
thName :: ByteString -> IO Thunk
thName n = newWHNFThunk (VStr n)

-- | Build @IntegerL n@.
integerL :: Int64 -> IO Thunk
integerL n = do
    nt <- newWHNFThunk (VInt n)
    newWHNFThunk (VCon "IntegerL" [nt])

-- | Build @CharL c@.
charL :: Char -> IO Thunk
charL c = do
    ct <- newWHNFThunk (VChar c)
    newWHNFThunk (VCon "CharL" [ct])

-- | Build @StringL s@ where @s@ is a [Char] list value.
stringL :: Val -> IO Thunk
stringL listVal = do
    lt <- newWHNFThunk listVal
    newWHNFThunk (VCon "StringL" [lt])

-- | Build @LitE lit@.
litE :: Thunk -> IO Thunk
litE litT = newWHNFThunk (VCon "LitE" [litT])

-- | Build @ConE name@.
conE :: ByteString -> IO Thunk
conE n = do
    nt <- thName n
    newWHNFThunk (VCon "ConE" [nt])

-- | Build @AppE f x@.
appE :: Thunk -> Thunk -> IO Thunk
appE fT xT = newWHNFThunk (VCon "AppE" [fT, xT])

-- | Build @ListE [e1, e2, ...]@ as a VCon "ListE" containing a [Thunk] list.
listE :: [Thunk] -> IO Thunk
listE ts = do
    listVal <- buildThunkList ts
    lt <- newWHNFThunk listVal
    newWHNFThunk (VCon "ListE" [lt])

-- | Build @TupE [e1, e2, ...]@ — same list encoding as ListE.
tupE :: [Thunk] -> IO Thunk
tupE ts = do
    listVal <- buildThunkList ts
    lt <- newWHNFThunk listVal
    newWHNFThunk (VCon "TupE" [lt])

-- | Build a VCon ":" / "[]" list from a list of thunks.
buildThunkList :: [Thunk] -> IO Val
buildThunkList []     = pure (VCon "[]" [])
buildThunkList (x:xs) = do
    tailVal <- buildThunkList xs
    tailT   <- newWHNFThunk tailVal
    pure (VCon ":" [x, tailT])

--------------------------------------------------------------------------------
-- Lift instances
--------------------------------------------------------------------------------

-- | @lift :: Lift a => a -> Exp@ — the master dispatch function.
-- Takes a Val and returns a Val encoding a TH Exp.
liftVal :: Val -> IO Val
liftVal (VInt n)  = do
    litT <- integerL n
    force =<< litE litT
liftVal (VChar c) = do
    litT <- charL c
    force =<< litE litT
liftVal (VStr bs) = do
    -- VStr is a raw ByteString; convert to [Char] list, then StringL
    let chars = BC.unpack bs
    charListVal <- buildCharList chars
    litT <- stringL charListVal
    force =<< litE litT
  where
    buildCharList []     = pure (VCon "[]" [])
    buildCharList (c:cs) = do
        h    <- newWHNFThunk (VChar c)
        rest <- buildCharList cs
        t    <- newWHNFThunk rest
        pure (VCon ":" [h, t])
liftVal VUnit = do
    -- () -> ConE "()"
    force =<< conE "()"
liftVal (VCon "True" []) = force =<< conE "True"
liftVal (VCon "False" []) = force =<< conE "False"
liftVal (VCon "[]" []) = do
    -- [] -> ListE []
    force =<< listE []
liftVal (VCon ":" [hT, tT]) = do
    -- list: collect all elements and build ListE
    hV <- force hT
    tV <- force tT
    elems <- collectList hV tV
    elemTs <- mapM (newWHNFThunk <=< liftVal) elems
    force =<< listE elemTs
  where
    collectList h (VCon "[]" []) = pure [h]
    collectList h (VCon ":" [hT', tT']) = do
        h' <- force hT'
        t' <- force tT'
        rest <- collectList h' t'
        pure (h : rest)
    collectList h other =
        throwTH ("liftVal: unexpected list tail: " <> showValForDebug other)
liftVal (VCon "Nothing" []) = force =<< conE "Nothing"
liftVal (VCon "Just" [xT]) = do
    xV <- force xT
    xLifted <- liftVal xV
    xE <- newWHNFThunk xLifted
    jE <- conE "Just"
    force =<< appE jE xE
liftVal (VCon "Left" [xT]) = do
    xV <- force xT
    xLifted <- liftVal xV
    xE <- newWHNFThunk xLifted
    lE <- conE "Left"
    force =<< appE lE xE
liftVal (VCon "Right" [xT]) = do
    xV <- force xT
    xLifted <- liftVal xV
    xE <- newWHNFThunk xLifted
    rE <- conE "Right"
    force =<< appE rE xE
liftVal v@(VCon name args)
    | isTupleName name = do
        -- Tuple: TupE [lift a, lift b, ...]
        argVals <- mapM force args
        argLifted <- mapM liftVal argVals
        argTs <- mapM newWHNFThunk argLifted
        force =<< tupE argTs
    | otherwise = do
        -- User-defined constructor: AppE (AppE (ConE "Foo") arg1) arg2 ...
        argVals <- mapM force args
        argExprs <- mapM liftVal argVals
        argTs <- mapM newWHNFThunk argExprs
        baseT <- conE name
        applyArgs baseT argTs
  where
    isTupleName n =
        BC.length n >= 2 &&
        BC.head n == '(' &&
        BC.last n == ')' &&
        BC.all (\c -> c == ',' || c == '(' || c == ')') n
    applyArgs acc [] = force acc
    applyArgs acc (t:ts) = do
        newAcc <- appE acc t
        applyArgs newAcc ts
liftVal v = throwTH ("liftVal: unsupported value: " <> showValForDebug v)

-- | The builtin @lift@ VFun.
liftBuiltin :: IO Val
liftBuiltin = pure $ VFun $ \argThunk -> do
    v <- force argThunk
    liftVal v

--------------------------------------------------------------------------------
-- thExpToExpr — decode a TH Exp Val back into IHC.AST.Expr
--------------------------------------------------------------------------------

-- | Decode a TH 'Exp' value-tree into an 'Expr'. Partial — unsupported
-- constructors throw a 'THError'.
thExpToExpr :: Val -> IO Expr
thExpToExpr (VCon "LitE" [litT]) = do
    litV <- force litT
    decodeLit litV
thExpToExpr (VCon "VarE" [nameT]) = do
    nameV <- force nameT
    n <- decodeName nameV
    pure (EVar n)
thExpToExpr (VCon "ConE" [nameT]) = do
    nameV <- force nameT
    n <- decodeName nameV
    pure (EVar n)
thExpToExpr (VCon "AppE" [fT, xT]) = do
    fV <- force fT
    xV <- force xT
    fE <- thExpToExpr fV
    xE <- thExpToExpr xV
    pure (EApp fE xE)
thExpToExpr (VCon "ListE" [listT]) = do
    listV <- force listT
    exprs <- decodeList listV thExpToExpr
    pure (buildListExpr exprs)
  where
    buildListExpr []     = EVar "[]"
    buildListExpr (e:es) = EApp (EApp (EVar ":") e) (buildListExpr es)
thExpToExpr (VCon "TupE" [listT]) = do
    listV <- force listT
    exprs <- decodeList listV (thMaybeExpToExpr "TupE")
    pure (ETuple exprs)
thExpToExpr (VCon "InfixE" [mLT, opT, mRT]) = do
    -- InfixE (Just l) op (Just r) = l `op` r
    mLV <- force mLT
    opV <- force opT
    mRV <- force mRT
    lE  <- decodeMaybeExpr "InfixE left" mLV
    opE <- thExpToExpr opV
    rE  <- decodeMaybeExpr "InfixE right" mRV
    opName <- case opE of
        EVar n -> pure n
        _      -> throwTH "InfixE: operator must be VarE"
    pure (EApp (EApp (EVar opName) lE) rE)
thExpToExpr (VCon name _) =
    throwTH ("thExpToExpr: unsupported TH Exp constructor: " <> BC.unpack name)
thExpToExpr v =
    throwTH ("thExpToExpr: expected a TH Exp VCon, got: " <> showValForDebug v)

-- | Like 'thExpToExpr' but decodes a @Maybe Exp@ (used by TupE elements
-- and InfixE). In TH, 'TupE' takes @[Maybe Exp]@.
thMaybeExpToExpr :: String -> Val -> IO Expr
thMaybeExpToExpr _ctx (VCon "Just" [eT]) = do
    eV <- force eT
    thExpToExpr eV
thMaybeExpToExpr _ctx v = thExpToExpr v  -- tolerate plain Exp too

decodeMaybeExpr :: String -> Val -> IO Expr
decodeMaybeExpr ctx (VCon "Just" [eT]) = do
    eV <- force eT
    thExpToExpr eV
decodeMaybeExpr ctx (VCon "Nothing" []) =
    throwTH ("decodeMaybeExpr: Nothing in " <> ctx)
decodeMaybeExpr ctx v = do
    -- Might be a bare Exp rather than Maybe Exp
    thExpToExpr v

decodeLit :: Val -> IO Expr
decodeLit (VCon "IntegerL" [nT]) = do
    nV <- force nT
    case nV of
        VInt n  -> pure (ELit (LInt n))
        VFloat d -> pure (ELit (LInt (round d)))
        other    -> throwTH ("IntegerL: expected VInt, got " <> showValForDebug other)
decodeLit (VCon "CharL" [cT]) = do
    cV <- force cT
    case cV of
        VChar c -> pure (ELit (LChar c))
        other   -> throwTH ("CharL: expected VChar, got " <> showValForDebug other)
decodeLit (VCon "StringL" [sT]) = do
    sV <- force sT
    case sV of
        VStr bs     -> pure (stringToConsList (BC.unpack bs))
        VCon "[]" _ -> pure (EVar "[]")
        VCon ":" _  -> do
            -- [Char] list
            chars <- extractChars sV
            pure (stringToConsList chars)
        other -> throwTH ("StringL: unexpected value " <> showValForDebug other)
decodeLit (VCon name _) =
    throwTH ("decodeLit: unsupported TH Lit constructor: " <> BC.unpack name)
decodeLit v =
    throwTH ("decodeLit: expected a TH Lit VCon, got: " <> showValForDebug v)

decodeName :: Val -> IO Name
decodeName (VStr bs) = pure bs
decodeName (VCon n _) = pure n   -- sometimes names are stored as VCon
decodeName v = throwTH ("decodeName: expected VStr, got " <> showValForDebug v)

decodeList :: Val -> (Val -> IO a) -> IO [a]
decodeList (VCon "[]" []) _ = pure []
decodeList (VCon ":" [hT, tT]) f = do
    hV <- force hT
    tV <- force tT
    x  <- f hV
    xs <- decodeList tV f
    pure (x : xs)
decodeList v _ =
    throwTH ("decodeList: expected list VCon, got: " <> showValForDebug v)

extractChars :: Val -> IO String
extractChars (VCon "[]" []) = pure []
extractChars (VCon ":" [hT, tT]) = do
    hV <- force hT
    tV <- force tT
    case hV of
        VChar c -> (c :) <$> extractChars tV
        other   -> throwTH ("extractChars: expected VChar, got " <> showValForDebug other)
extractChars v = throwTH ("extractChars: expected list, got " <> showValForDebug v)

-- | Convert a Haskell String to a cons-list of LChar expressions,
-- matching the encoding in IHC.Parser.
stringToConsList :: String -> Expr
stringToConsList []     = EVar "[]"
stringToConsList (c:cs) = EApp (EApp (EVar ":") (ELit (LChar c))) (stringToConsList cs)

--------------------------------------------------------------------------------
-- expandSplicesInExpr
--------------------------------------------------------------------------------

-- | Walk an 'Expr' and expand every 'ESplice' in place.
-- Hard-caps at depth 16 to prevent infinite splice loops.
expandSplicesInExpr :: Env -> ImplicitParamMap -> Int -> Expr -> IO Expr
expandSplicesInExpr env ipm depth expr
    | depth > 16 = throwTH "splice expansion exceeded depth limit 16"
    | otherwise  = go expr
  where
    recur = expandSplicesInExpr env ipm (depth + 1)

    go (ESplice inner) = do
        -- Evaluate the splice expression to a TH Exp value.
        innerExpanded <- recur inner
        thVal <- eval env ipm innerExpanded
        -- Decode the TH Exp into an IHC Expr.
        resultExpr <- thExpToExpr thVal
        -- Re-traverse in case the result contains nested splices.
        recur resultExpr

    go (EVar n)  = pure (EVar n)
    go (ELit l)  = pure (ELit l)
    go (EApp f x) = EApp <$> go f <*> go x
    go (ELam n e) = ELam n <$> go e
    go (ELet bs e) = do
        bs' <- mapM (\(n, b) -> (n,) <$> go b) bs
        e'  <- go e
        pure (ELet bs' e')
    go (ECase s as) = do
        s'  <- go s
        as' <- mapM goAlt as
        pure (ECase s' as')
    go (EIf c t e) = EIf <$> go c <*> go t <*> go e
    go (EDo stmts) = EDo <$> mapM goStmt stmts
    go (ENeg e) = ENeg <$> go e
    go (ETuple es) = ETuple <$> mapM go es
    go (EImplicitRef n) = pure (EImplicitRef n)
    go (EImplicitLet bs e) = do
        bs' <- mapM (\(n, b) -> (n,) <$> go b) bs
        e'  <- go e
        pure (EImplicitLet bs' e')
    go (ERecordCon n fields) = do
        fields' <- mapM (\(fn, fe) -> (fn,) <$> go fe) fields
        pure (ERecordCon n fields')
    go (ERecordWild n) = pure (ERecordWild n)
    go (ERecordUpdate e fields) = do
        e'      <- go e
        fields' <- mapM (\(fn, fe) -> (fn,) <$> go fe) fields
        pure (ERecordUpdate e' fields')
    go (ELabel n) = pure (ELabel n)   -- Phase 3.5: labels are self-contained
    -- Phase 2.12: EQuote is a leaf in the splice-expansion pass.
    -- Its body is NOT expanded (it's a quotation, not a splice).
    go (EQuote e) = pure (EQuote e)
    -- Value-level @T: recurse into the inner expression; the type arg is opaque.
    go (ETyApp e ty) = do
        e' <- go e
        pure (ETyApp e' ty)

    goAlt (Alt p e) = Alt p <$> go e

    goStmt (SExpr e)   = SExpr <$> go e
    goStmt (SBind n e) = SBind n <$> go e
    goStmt (SLet bs)   = SLet <$> mapM (\(n, b) -> (n,) <$> go b) bs

--------------------------------------------------------------------------------
-- exprToVal — quotation: Expr -> TH Exp Val  (Phase 2.12)
--
-- Walks an IHC AST Expr and encodes it as a TH Exp-shaped Val.
-- This is the runtime implementation of [| expr |] brackets.
-- The encoding matches what liftVal / thExpToExpr use, so
-- $( [| expr |] ) round-trips correctly.
--------------------------------------------------------------------------------

-- | Convert an IHC 'Expr' (the body of a [| ... |] bracket) into
-- a 'Val' encoding the corresponding TH 'Exp'. The result is the
-- same encoding as 'liftVal' / 'liftBuiltin' produces.
exprToVal :: Expr -> IO Val
exprToVal (EVar n)
    -- Capitalised name → ConE; lowercase/operator → VarE.
    | not (BC.null n) && BC.head n >= 'A' && BC.head n <= 'Z' = do
        nt <- thName n
        force =<< newWHNFThunk (VCon "ConE" [nt])
    | otherwise = do
        nt <- thName n
        force =<< newWHNFThunk (VCon "VarE" [nt])
exprToVal (ELit (LInt n)) = do
    litT <- integerL n
    force =<< litE litT
exprToVal (ELit (LFloat d)) = do
    -- Encode floating-point as IntegerL (round) — RationalL needs more infra.
    litT <- integerL (round d)
    force =<< litE litT
exprToVal (ELit (LStr bs)) = do
    charListVal <- buildCharList (BC.unpack bs)
    litT <- stringL charListVal
    force =<< litE litT
  where
    buildCharList []     = pure (VCon "[]" [])
    buildCharList (c:cs) = do
        h    <- newWHNFThunk (VChar c)
        rest <- buildCharList cs
        t    <- newWHNFThunk rest
        pure (VCon ":" [h, t])
exprToVal (ELit (LChar c)) = do
    litT <- charL c
    force =<< litE litT
exprToVal (EApp f x) = do
    fV  <- exprToVal f
    xV  <- exprToVal x
    fT  <- newWHNFThunk fV
    xT  <- newWHNFThunk xV
    force =<< appE fT xT
exprToVal (ETuple es) = do
    elemVals <- mapM exprToVal es
    elemTs   <- mapM newWHNFThunk elemVals
    force =<< tupE elemTs
exprToVal (ENeg e) = do
    -- negate x  =  AppE (VarE "negate") x
    negT <- do
        nt <- thName "negate"
        newWHNFThunk (VCon "VarE" [nt])
    xV   <- exprToVal e
    xT   <- newWHNFThunk xV
    force =<< appE negT xT
-- Value-level @T: the type argument is opaque metadata. Encode the inner
-- expression as the TH Exp; a future splice pass that cares about type
-- applications can inspect the 'ETyApp' node before reaching here.
exprToVal (ETyApp e _ty) = exprToVal e
-- For other unsupported forms, emit a VarE "<unsupported>" placeholder.
exprToVal _ =
    force =<< newWHNFThunk (VCon "VarE" [])

--------------------------------------------------------------------------------
-- Builtin name pairs to register in the environment
--------------------------------------------------------------------------------

-- | (name, IO Val) pairs for all TH-related builtins.
-- Registers under both short and fully-qualified paths.
thBuiltinPairs :: [(ByteString, IO Val)]
thBuiltinPairs =
    [ ("lift",                                liftBuiltin)
    , ("TH.lift",                             liftBuiltin)
    , ("Language.Haskell.TH.lift",            liftBuiltin)
    , ("Language.Haskell.TH.Syntax.lift",     liftBuiltin)
    , ("Language.Haskell.TH.Lib.lift",        liftBuiltin)
    ]

