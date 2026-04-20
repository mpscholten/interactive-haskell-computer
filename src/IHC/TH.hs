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
    -- * Phase 2.13: top-level splice decl decoder
    , thDecsToBindings
    , thExpandSpliceDecl
    , resetNewNameCounter
    ) where

import Control.Exception (throwIO, Exception)
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Int (Int64)
import System.IO.Unsafe (unsafePerformIO)
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
decodeName (VCon "Name" [nT]) = do
    -- @VCon "Name" [VStr bs]@ — the shape produced by 'mkNameBuiltin'.
    nV <- force nT
    case nV of
        VStr bs    -> pure bs
        VCon bs [] -> pure bs
        other      -> decodeName other
decodeName (VCon "NameU" [nT]) = do
    nV <- force nT; decodeName nV
decodeName (VCon "NameS" [nT]) = do
    nV <- force nT; decodeName nV
decodeName (VCon "OccName" [nT]) = do
    nV <- force nT; decodeName nV
decodeName (VCon n _) = pure n   -- sometimes names are stored as VCon tag
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
    go (ETypedMethod cls method tag) = pure (ETypedMethod cls method tag)

    goAlt (Alt p e) = Alt p <$> go e

    goStmt (SExpr e)   = SExpr <$> go e
    goStmt (SBind n e) = SBind n <$> go e
    goStmt (SLet bs)   = SLet <$> mapM (\(n, b) -> (n,) <$> go b) bs
    goStmt (SImplicitLet bs) = SImplicitLet <$> mapM (\(n, b) -> (n,) <$> go b) bs

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
    -- Phase 2.13: Q monad + Name primitives for top-level splices.
    , ("mkName",                              mkNameBuiltin)
    , ("Language.Haskell.TH.mkName",          mkNameBuiltin)
    , ("Language.Haskell.TH.Syntax.mkName",   mkNameBuiltin)
    , ("newName",                             newNameBuiltin)
    , ("Language.Haskell.TH.newName",         newNameBuiltin)
    , ("Language.Haskell.TH.Syntax.newName",  newNameBuiltin)
    , ("runQ",                                runQBuiltin)
    , ("Language.Haskell.TH.runQ",            runQBuiltin)
    , ("Language.Haskell.TH.Syntax.runQ",     runQBuiltin)
    , ("reify",                               reifyBuiltin)
    , ("Language.Haskell.TH.reify",           reifyBuiltin)
    , ("Language.Haskell.TH.Syntax.reify",    reifyBuiltin)
    ]
    -- Phase 2.13: TH AST constructors.  These have no Haskell source in
    -- our cache (the template-haskell package isn't source-loaded) and
    -- the decoder expects VCon-shaped values, so we auto-generate
    -- curried constructor builtins for the shapes deriveJSON emits.
    ++ thConstructorPairs

-- | Every @(name, arity)@ pair for the Template Haskell constructors we
-- support as builtin shims.  Registered under the bare name and the
-- fully-qualified @Language.Haskell.TH.*@ path so imports resolve.
thConstructorCatalog :: [(ByteString, Int)]
thConstructorCatalog =
    -- Dec
    [ ("ValD", 3)           -- pat, body, wheres
    , ("FunD", 2)           -- name, clauses
    , ("SigD", 2)           -- name, type
    , ("InstanceD", 4)      -- overlap, ctx, typ, decs
    , ("DataD", 6)
    , ("NewtypeD", 6)
    , ("ClassD", 4)
    , ("PragmaD", 1)
    -- Clause (fun body form: Clause [pat] body [dec])
    , ("Clause", 3)
    -- Body
    , ("NormalB", 1)
    , ("GuardedB", 1)
    -- Exp
    , ("VarE", 1)
    , ("ConE", 1)
    , ("LitE", 1)
    , ("AppE", 2)
    , ("AppTypeE", 2)
    , ("InfixE", 3)
    , ("UInfixE", 3)
    , ("ParensE", 1)
    , ("LamE", 2)
    , ("LamCaseE", 1)
    , ("TupE", 1)
    , ("UnboxedTupE", 1)
    , ("CondE", 3)
    , ("MultiIfE", 1)
    , ("LetE", 2)
    , ("CaseE", 2)
    , ("DoE", 2)
    , ("CompE", 1)
    , ("ArithSeqE", 1)
    , ("ListE", 1)
    , ("SigE", 2)
    , ("RecConE", 2)
    , ("RecUpdE", 2)
    , ("StaticE", 1)
    , ("UnboundVarE", 1)
    , ("LabelE", 1)
    , ("ImplicitParamVarE", 1)
    -- Pat
    , ("VarP", 1)
    , ("ConP", 3)        -- name, [type], [pat] (modern) / (name, [pat])
    , ("LitP", 1)
    , ("TupP", 1)
    , ("UnboxedTupP", 1)
    , ("InfixP", 3)
    , ("UInfixP", 3)
    , ("ParensP", 1)
    , ("TildeP", 1)
    , ("BangP", 1)
    , ("AsP", 2)
    , ("RecP", 2)
    , ("ListP", 1)
    , ("SigP", 2)
    , ("ViewP", 2)
    , ("WildP", 0)
    -- Lit
    , ("IntegerL", 1)
    , ("RationalL", 1)
    , ("CharL", 1)
    , ("StringL", 1)
    , ("IntPrimL", 1)
    , ("WordPrimL", 1)
    , ("FloatPrimL", 1)
    , ("DoublePrimL", 1)
    , ("StringPrimL", 1)
    , ("CharPrimL", 1)
    -- Type
    , ("ConT", 1)
    , ("VarT", 1)
    , ("AppT", 2)
    , ("ArrowT", 0)
    , ("ListT", 0)
    , ("TupleT", 1)
    , ("ForallT", 3)
    , ("PromotedT", 1)
    , ("LitT", 1)
    , ("StarT", 0)
    -- TyVarBndr
    , ("PlainTV", 1)
    , ("KindedTV", 2)
    -- Info (returned by reify)
    , ("TyConI", 1)
    , ("ClassI", 2)
    , ("ClassOpI", 3)
    , ("VarI", 3)
    , ("PrimTyConI", 3)
    , ("FamilyI", 2)
    -- Name wrapper
    -- NOTE: @Name@ is used as VCon "Name" [VStr bytes] by mkName.
    -- We don't register it here because the parser treats @Name@
    -- uses as a type, and user code doesn't apply "Name" as a ctor.
    ]

-- | Flatten the catalog into @(name, IO Val)@ pairs for 'thBuiltinPairs'.
thConstructorPairs :: [(ByteString, IO Val)]
thConstructorPairs =
    concatMap oneCtor thConstructorCatalog
  where
    oneCtor (name, arity) =
        let mkV = pure (buildCtor name arity)
        in [ (name,                                    mkV)
           , ("Language.Haskell.TH." <> name,          mkV)
           , ("Language.Haskell.TH.Syntax." <> name,   mkV)
           , ("Language.Haskell.TH.Lib." <> name,      mkV)
           ]

    buildCtor :: Name -> Int -> Val
    buildCtor name 0    = VCon name []
    buildCtor name n    = buildLam name n []

    buildLam :: Name -> Int -> [Thunk] -> Val
    buildLam name 0    acc = VCon name (reverse acc)
    buildLam name left acc = VFun $ \t ->
        pure (buildLam name (left - 1) (t : acc))

--------------------------------------------------------------------------------
-- Phase 2.13: Q monad + Name primitives
--
-- The @Q@ monad is represented as @VIO@ at the Val level.  It is simply a
-- suspended @IO Val@ action; the @return@/@>>=@ machinery is whatever the
-- interpreter uses for plain @IO@.  We encode Template Haskell Names as
-- @VCon "Name" [VStr bytes]@ to match the constructor that the decoder
-- expects.
--------------------------------------------------------------------------------

-- | Global counter for 'newName'. Reset once per load via
-- 'resetNewNameCounter'.
{-# NOINLINE newNameCounterRef #-}
newNameCounterRef :: IORef Int
newNameCounterRef = unsafePerformIO (newIORef 0)

resetNewNameCounter :: IO ()
resetNewNameCounter = writeIORef newNameCounterRef 0

-- | @mkName :: String -> Name@.  Returns @VCon "Name" [VStr bytes]@.
mkNameBuiltin :: IO Val
mkNameBuiltin = pure $ VFun $ \argT -> do
    v <- force argT
    bs <- valToBytes v
    bsT <- newWHNFThunk (VStr bs)
    pure (VCon "Name" [bsT])

-- | @newName :: String -> Q Name@.  Gensym by appending @_N@.
newNameBuiltin :: IO Val
newNameBuiltin = pure $ VFun $ \argT -> do
    v <- force argT
    base <- valToBytes v
    pure $ VIO $ do
        n <- atomicModifyIORef' newNameCounterRef (\c -> (c + 1, c))
        let unique = base <> BC.pack ("_" <> show n)
        uT <- newWHNFThunk (VStr unique)
        pure (VCon "Name" [uT])

-- | @runQ :: Quasi m => Q a -> m a@.  Identity on 'VIO' values.
runQBuiltin :: IO Val
runQBuiltin = pure $ VFun $ \argT -> do
    v <- force argT
    case v of
        VIO _ -> pure v
        other -> pure (VIO (pure other))

-- | @reify :: Name -> Q Info@.  Phase-2.13 stub.
reifyBuiltin :: IO Val
reifyBuiltin = pure $ VFun $ \argT -> do
    _ <- force argT
    pure $ VIO $ do
        nameT  <- thName (BC.pack "<reify-stub>")
        let nameV = VCon "Name" [nameT]
        nameT2 <- newWHNFThunk nameV
        emptyCtx <- newWHNFThunk (VCon "[]" [])
        emptyTvs <- newWHNFThunk (VCon "[]" [])
        nothing  <- newWHNFThunk (VCon "Nothing" [])
        emptyCtors  <- newWHNFThunk (VCon "[]" [])
        emptyDerivs <- newWHNFThunk (VCon "[]" [])
        dataDT <- newWHNFThunk
                    (VCon "DataD"
                        [emptyCtx, nameT2, emptyTvs, nothing, emptyCtors, emptyDerivs])
        pure (VCon "TyConI" [dataDT])

-- | Convert a String-shaped 'Val' into a 'ByteString'.
valToBytes :: Val -> IO ByteString
valToBytes (VStr bs) = pure bs
valToBytes (VCon "Name" [nT]) = do
    nV <- force nT
    case nV of
        VStr bs -> pure bs
        _       -> valToBytes nV
valToBytes v = do
    cs <- collectChars v
    pure (BC.pack cs)
  where
    collectChars (VCon "[]" []) = pure []
    collectChars (VCon ":" [hT, tT]) = do
        hV <- force hT
        tV <- force tT
        case hV of
            VChar c -> (c :) <$> collectChars tV
            _       -> throwTH ("valToBytes: expected VChar, got "
                                 <> showValForDebug hV)
    collectChars other =
        throwTH ("valToBytes: expected string-shaped value, got "
                 <> showValForDebug other)

--------------------------------------------------------------------------------
-- Phase 2.13: TH Dec -> IHC binding decoder
--------------------------------------------------------------------------------

-- | Evaluate a top-level splice expression and decode its result into
-- a list of @(name, body)@ top-level bindings.
thExpandSpliceDecl :: Env -> ImplicitParamMap -> Expr -> IO [(Name, Expr)]
thExpandSpliceDecl env ipm spliceExpr = do
    v0 <- eval env ipm spliceExpr
    decsVal <- unwrapQ v0
    thDecsToBindings decsVal

-- | Transparent @VIO@ unwrapper.
unwrapQ :: Val -> IO Val
unwrapQ (VIO act) = act >>= unwrapQ
unwrapQ v         = pure v

-- | Decode @[Dec]@ → @[(Name, Expr)]@.
thDecsToBindings :: Val -> IO [(Name, Expr)]
thDecsToBindings (VCon "[]" []) = pure []
thDecsToBindings (VCon ":" [hT, tT]) = do
    hV <- force hT
    tV <- force tT
    mThis <- thDecToBinding hV
    rest  <- thDecsToBindings tV
    pure $ case mThis of
        Nothing -> rest
        Just b  -> b : rest
thDecsToBindings v =
    throwTH ("thDecsToBindings: expected a [Dec] list, got " <> showValForDebug v)

-- | Decode a single TH 'Dec' into a value-binding pair; 'Nothing' if the
-- 'Dec' doesn't bind a runtime value (e.g. a type sig).
thDecToBinding :: Val -> IO (Maybe (Name, Expr))
thDecToBinding dec = case dec of
    VCon "ValD" [patT, bodyT, _decsT] -> do
        patV <- force patT
        bodyV <- force bodyT
        name <- decodeVarP patV
        body <- decodeBody bodyV
        pure (Just (name, body))
    VCon "FunD" [nameT, clausesT] -> do
        nameV <- force nameT
        name <- decodeName nameV
        clausesV <- force clausesT
        body <- decodeClauses clausesV
        pure (Just (name, body))
    VCon "SigD" _ -> pure Nothing
    VCon "InstanceD" _ -> pure Nothing
    VCon "DataD" _ -> pure Nothing
    VCon "NewtypeD" _ -> pure Nothing
    VCon "ClassD" _ -> pure Nothing
    VCon "PragmaD" _ -> pure Nothing
    VCon n _ ->
        throwTH ("thDecToBinding: unsupported Dec constructor: " <> BC.unpack n)
    other ->
        throwTH ("thDecToBinding: expected a Dec VCon, got "
                 <> showValForDebug other)
  where
    decodeVarP (VCon "VarP" [nameT]) = do
        nameV <- force nameT
        decodeName nameV
    decodeVarP v =
        throwTH ("thDecToBinding: ValD LHS must be VarP, got "
                 <> showValForDebug v)

    decodeBody (VCon "NormalB" [eT]) = do
        eV <- force eT
        thExpToExpr eV
    decodeBody (VCon "GuardedB" _) =
        throwTH "thDecToBinding: GuardedB (guards) not yet supported"
    decodeBody v =
        throwTH ("thDecToBinding: expected NormalB, got " <> showValForDebug v)

    decodeClauses (VCon "[]" []) =
        throwTH "thDecToBinding: FunD has no clauses"
    decodeClauses (VCon ":" [clT, restT]) = do
        clV <- force clT
        restV <- force restT
        case restV of
            VCon "[]" [] -> decodeOneClause clV
            _ -> throwTH "thDecToBinding: FunD with multiple clauses not yet supported"
    decodeClauses v =
        throwTH ("thDecToBinding: FunD clauses must be a list, got "
                 <> showValForDebug v)

    decodeOneClause (VCon "Clause" [patsT, bodyT, _decsT]) = do
        patsV <- force patsT
        bodyV <- force bodyT
        pats <- decodePats patsV
        body <- decodeBody bodyV
        pure (foldr wrapLam body pats)
    decodeOneClause v =
        throwTH ("thDecToBinding: expected Clause VCon, got "
                 <> showValForDebug v)

    wrapLam (PVar n) e = ELam n e
    wrapLam p       e =
        ELam (BC.pack "__thArg") (ECase (EVar (BC.pack "__thArg")) [Alt p e])

    decodePats (VCon "[]" []) = pure []
    decodePats (VCon ":" [hT, tT]) = do
        hV <- force hT
        tV <- force tT
        p  <- decodePat hV
        ps <- decodePats tV
        pure (p : ps)
    decodePats v =
        throwTH ("thDecToBinding: expected [Pat], got " <> showValForDebug v)

    decodePat (VCon "VarP" [nT]) = do
        nV <- force nT
        PVar <$> decodeName nV
    decodePat (VCon "WildP" []) = pure PWild
    decodePat (VCon "LitP" [lT]) = do
        lV <- force lT
        case lV of
            VCon "IntegerL" [iT] -> do
                iV <- force iT
                case iV of
                    VInt n -> pure (PLit (LInt n))
                    _ -> throwTH "LitP: IntegerL payload must be VInt"
            VCon "CharL" [cT] -> do
                cV <- force cT
                case cV of
                    VChar c -> pure (PLit (LChar c))
                    _ -> throwTH "LitP: CharL payload must be VChar"
            _ -> throwTH "LitP: unsupported literal"
    decodePat v =
        throwTH ("thDecToBinding: unsupported pattern " <> showValForDebug v)

