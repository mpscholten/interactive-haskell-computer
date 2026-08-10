-- | Template Haskell quotation/splice evaluation and AST decoding.
--
-- This module provides:
--
-- 1. @thExpToExpr :: Val -> IO Expr@ — the anti-quoter that walks a
--    TH 'Exp' value-tree and produces an 'IHC.AST.Expr'.
--
-- 2. @expandSplicesInExpr :: Env -> Int -> Expr -> IO Expr@ — recursively
--    replaces every 'ESplice' node with the 'Expr' it evaluates to.
--    Hard-caps recursion at depth 16.
--
-- Ordinary template-haskell modules, functions, classes, instances, and AST
-- constructors are source-loaded. This module exposes no host-backed TH names.
module IHC.TH
    ( thExpToExpr
    , expandSplicesInExpr
    , thBuiltinPairs
    , exprToVal
    -- * Phase 2.13: top-level splice decl decoder
    , thDecsToBindings
    , thExpandSpliceDecl
    ) where

import Control.Exception (throwIO, Exception, SomeException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Int (Int64)
import qualified Data.Set as Set
import IHC.AST
import IHC.Classes
    ( legacyHooks, getSharedClassReg, triggerCoreInstanceLoadForTag
    , drainCataloguedInstancesForClass )
import IHC.ConstructorMetadata (globalConstructorTypeRegistryRef)
import qualified IHC.Elaborate as Elab
import IHC.Eval (eval, force, currentOwner)
import IHC.TypeAST (Type(..))
import IHC.TypeGlobals (globalTypeSigsRef, globalTypeSynonymsRef)
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

-- | Build @AppE f x@.
appE :: Thunk -> Thunk -> IO Thunk
appE fT xT = newWHNFThunk (VCon "AppE" [fT, xT])

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

--------------------------------------------------------------------------------
-- thExpToExpr — decode a TH Exp Val back into IHC.AST.Expr
--------------------------------------------------------------------------------

-- | Decode a TH 'Exp' value-tree into an 'Expr'. Partial — unsupported
-- constructors throw a 'THError'.
thExpToExpr :: Val -> IO Expr
thExpToExpr value = runOneQExp value >>= decodeTHExp

-- | Consume exactly one source or host @Q Exp@ layer at the point where a
-- Template Haskell expression enters the interpreter.  Quasiquoters and
-- splices share this boundary: source @Q@ is a newtype around a
-- rank-polymorphic @Quasi m => m a@ action, so merely applying 'runIOVal' to
-- the outer constructor cannot execute it.  We choose the existing IO-backed
-- Quasi runner and elaborate the stored action at @IO Exp@.
--
-- Deliberately do not recurse here: @Q (Q Exp)@ must retain its inner layer.
runOneQExp :: Val -> IO Val
runOneQExp (VIO action) = action
runOneQExp (VCon "Q" [actionT]) = do
    state <- readIORef actionT
    value <- case state of
        Unevaluated (Closure actionEnv actionIpm actionExpr) -> do
            actionExpr' <- elaborateAt actionEnv
                (TyApp (TyCon "IO") (TyCon "Exp")) actionExpr
            eval legacyHooks actionEnv actionIpm actionExpr'
        _ -> force legacyHooks actionT
    case value of
        VIO action -> action
        other      -> pure other
runOneQExp value = pure value

-- Keep source-Q execution on the normal elaborator/provider path.  This is
-- the same expected-type operation used while expanding a splice; it is not
-- a catalogue of TH or library method names.
elaborateAt :: Env -> Type -> Expr -> IO Expr
elaborateAt targetEnv expectedTy inner = do
    mClassReg <- getSharedClassReg legacyHooks
    case mClassReg of
        Nothing -> pure inner
        Just classReg -> do
            sigs <- readIORef globalTypeSigsRef
            synonyms <- readIORef globalTypeSynonymsRef
            constructorTypes <- readIORef globalConstructorTypeRegistryRef
            owner <- currentOwner legacyHooks targetEnv
            result <- try (Elab.elaborateOwned classReg sigs synonyms
                constructorTypes owner (Elab.ExpectType expectedTy) inner)
                :: IO (Either SomeException (Expr, Type))
            case result of
                Left _ -> pure inner
                Right (elaborated, _) -> do
                    mapM_ materializeClass
                        (Set.toList (typedMethodClasses elaborated))
                    pure elaborated
  where
    materializeClass (cls, tag) = do
        triggerCoreInstanceLoadForTag legacyHooks cls tag
        _ <- drainCataloguedInstancesForClass cls
        pure ()

    typedMethodClasses = goClasses
      where
        goClasses (ETypedMethod cls _ tag) = Set.singleton (cls, tag)
        goClasses (EApp f x) = Set.union (goClasses f) (goClasses x)
        goClasses (ELam _ e) = goClasses e
        goClasses (ELet bs e) = Set.unions (goClasses e : map (goClasses . snd) bs)
        goClasses (ECase s as) = Set.unions (goClasses s : [goClasses e | Alt _ e <- as])
        goClasses (EIf c t e) = Set.unions [goClasses c, goClasses t, goClasses e]
        goClasses (EDo ss) = Set.unions (map stmtClasses ss)
        goClasses (ENeg e) = goClasses e
        goClasses (ETuple es) = Set.unions (map goClasses es)
        goClasses (EImplicitLet bs e) = Set.unions (goClasses e : map (goClasses . snd) bs)
        goClasses (ERecordCon _ fs) = Set.unions (map (goClasses . snd) fs)
        goClasses (ERecordUpdate e fs) = Set.unions (goClasses e : map (goClasses . snd) fs)
        goClasses (ESplice e) = goClasses e
        goClasses (ETyApp e _) = goClasses e
        goClasses (ELocalSig _ e) = goClasses e
        goClasses (EConstrainedValue e _) = goClasses e
        goClasses _ = Set.empty

        stmtClasses (SExpr e) = goClasses e
        stmtClasses (SBind _ e) = goClasses e
        stmtClasses (SBangBind _ e) = goClasses e
        stmtClasses (SLet bs) = Set.unions (map (goClasses . snd) bs)
        stmtClasses (SImplicitLet bs) = Set.unions (map (goClasses . snd) bs)

decodeTHExp :: Val -> IO Expr
decodeTHExp (VCon "LitE" [litT]) = do
    litV <- force legacyHooks litT
    decodeLit litV
decodeTHExp (VCon "VarE" [nameT]) = do
    nameV <- force legacyHooks nameT
    n <- decodeName nameV
    pure (EVar n)
decodeTHExp (VCon "ConE" [nameT]) = do
    nameV <- force legacyHooks nameT
    n <- decodeName nameV
    pure (EVar n)
decodeTHExp (VCon "AppE" [fT, xT]) = do
    fV <- force legacyHooks fT
    xV <- force legacyHooks xT
    fE <- thExpToExpr fV
    xE <- thExpToExpr xV
    pure (EApp fE xE)
decodeTHExp (VCon "ListE" [listT]) = do
    listV <- force legacyHooks listT
    exprs <- decodeList listV thExpToExpr
    pure (buildListExpr exprs)
  where
    buildListExpr []     = EVar "[]"
    buildListExpr (e:es) = EApp (EApp (EVar ":") e) (buildListExpr es)
decodeTHExp (VCon "TupE" [listT]) = do
    listV <- force legacyHooks listT
    exprs <- decodeList listV (thMaybeExpToExpr "TupE")
    pure (ETuple exprs)
decodeTHExp (VCon "InfixE" [mLT, opT, mRT]) = do
    -- InfixE (Just l) op (Just r) = l `op` r
    mLV <- force legacyHooks mLT
    opV <- force legacyHooks opT
    mRV <- force legacyHooks mRT
    lE  <- decodeMaybeExpr "InfixE left" mLV
    opE <- thExpToExpr opV
    rE  <- decodeMaybeExpr "InfixE right" mRV
    opName <- case opE of
        EVar n -> pure n
        _      -> throwTH "InfixE: operator must be VarE"
    pure (EApp (EApp (EVar opName) lE) rE)
decodeTHExp (VCon "GetFieldE" [recordT, fieldT]) = do
    recordV <- force legacyHooks recordT
    fieldV <- force legacyHooks fieldT
    recordE <- thExpToExpr recordV
    field <- decodeTHString "GetFieldE" fieldV
    pure (applyFieldProjection field recordE)
decodeTHExp (VCon "ProjectionE" [fieldsT]) = do
    fieldsV <- force legacyHooks fieldsT
    fields <- decodeNonEmptyStrings "ProjectionE" fieldsV
    -- TH's @(.a.b)@ applies fields from left to right.  The synthetic
    -- binder cannot collide with a source name because '$' is not valid
    -- at the start of an ordinary Haskell identifier.
    let argument = BC.pack "$thProjection"
    pure (ELam argument
        (foldl (flip applyFieldProjection) (EVar argument) fields))
decodeTHExp (VCon name _) =
    throwTH ("thExpToExpr: unsupported TH Exp constructor: " <> BC.unpack name)
decodeTHExp v =
    throwTH ("thExpToExpr: expected a TH Exp VCon, got: " <> showValForDebug v)

-- | Like 'thExpToExpr' but decodes a @Maybe Exp@ (used by TupE elements
-- and InfixE). In TH, 'TupE' takes @[Maybe Exp]@.
thMaybeExpToExpr :: String -> Val -> IO Expr
thMaybeExpToExpr _ctx (VCon "Just" [eT]) = do
    eV <- force legacyHooks eT
    thExpToExpr eV
thMaybeExpToExpr _ctx v = thExpToExpr v  -- tolerate plain Exp too

decodeMaybeExpr :: String -> Val -> IO Expr
decodeMaybeExpr ctx (VCon "Just" [eT]) = do
    eV <- force legacyHooks eT
    thExpToExpr eV
decodeMaybeExpr ctx (VCon "Nothing" []) =
    throwTH ("decodeMaybeExpr: Nothing in " <> ctx)
decodeMaybeExpr ctx v = do
    -- Might be a bare Exp rather than Maybe Exp
    thExpToExpr v

decodeLit :: Val -> IO Expr
decodeLit (VCon "IntegerL" [nT]) = do
    nV <- force legacyHooks nT
    case nV of
        VInt n  -> pure (ELit (LInt n))
        VFloat d -> pure (ELit (LInt (round d)))
        other    -> throwTH ("IntegerL: expected VInt, got " <> showValForDebug other)
decodeLit (VCon "CharL" [cT]) = do
    cV <- force legacyHooks cT
    case cV of
        VChar c -> pure (ELit (LChar c))
        other   -> throwTH ("CharL: expected VChar, got " <> showValForDebug other)
decodeLit (VCon "StringL" [sT]) = do
    sV <- force legacyHooks sT
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
    -- Source-loaded TH Name constructors retain their occurrence bytes here.
    nV <- force legacyHooks nT
    case nV of
        VStr bs    -> pure bs
        VCon bs [] -> pure bs
        other      -> decodeName other
decodeName (VCon "NameU" [nT]) = do
    nV <- force legacyHooks nT; decodeName nV
decodeName (VCon "NameS" [nT]) = do
    nV <- force legacyHooks nT; decodeName nV
decodeName (VCon "OccName" [nT]) = do
    nV <- force legacyHooks nT; decodeName nV
decodeName (VCon n _) = pure n   -- sometimes names are stored as VCon tag
decodeName v = throwTH ("decodeName: expected VStr, got " <> showValForDebug v)

decodeList :: Val -> (Val -> IO a) -> IO [a]
decodeList (VCon "[]" []) _ = pure []
decodeList (VCon ":" [hT, tT]) f = do
    hV <- force legacyHooks hT
    tV <- force legacyHooks tT
    x  <- f hV
    xs <- decodeList tV f
    pure (x : xs)
decodeList v _ =
    throwTH ("decodeList: expected list VCon, got: " <> showValForDebug v)

decodeTHString :: String -> Val -> IO ByteString
decodeTHString _ctx (VStr bs) = pure bs
decodeTHString _ctx chars@(VCon "[]" []) =
    BC.pack <$> extractChars chars
decodeTHString _ctx chars@(VCon ":" _) =
    BC.pack <$> extractChars chars
decodeTHString ctx v =
    throwTH (ctx <> ": expected String, got " <> showValForDebug v)

decodeNonEmptyStrings :: String -> Val -> IO [ByteString]
decodeNonEmptyStrings ctx (VCon ":|" [headT, tailT]) = do
    headV <- force legacyHooks headT
    tailV <- force legacyHooks tailT
    headField <- decodeTHString ctx headV
    tailFields <- decodeList tailV (decodeTHString ctx)
    pure (headField : tailFields)
decodeNonEmptyStrings ctx v =
    throwTH (ctx <> ": expected NonEmpty String, got " <> showValForDebug v)

applyFieldProjection :: ByteString -> Expr -> Expr
applyFieldProjection field =
    EApp (EVar (BC.pack "$fldProj$" <> field))

extractChars :: Val -> IO String
extractChars (VCon "[]" []) = pure []
extractChars (VCon ":" [hT, tT]) = do
    hV <- force legacyHooks hT
    tV <- force legacyHooks tT
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
        -- A splice boundary supplies the language-level expectation @Q Exp@.
        -- This lets the ordinary elaborator select source-defined class
        -- methods such as Applicative.pure without recognizing their names.
        -- Failure remains optimistic: unsupported TH/type syntax falls back
        -- to the unchanged expression and the evaluator reports the real
        -- runtime error.
        elaborated <- elaborateQExp innerExpanded
        thVal <- eval legacyHooks env ipm elaborated >>= unwrapOneQ
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
    -- QuasiQuoter body stays opaque here; expansion happens at eval time.
    go (EQuasiQuote n b) = pure (EQuasiQuote n b)
    -- Value-level @T: recurse into the inner expression; the type arg is opaque.
    go (ETyApp e ty) = do
        e' <- go e
        pure (ETyApp e' ty)
    go (ELocalSig ty e) = ELocalSig ty <$> go e
    go (ETypedMethod cls method tag) = pure (ETypedMethod cls method tag)
    go (EConstrainedValue e constraints) = do
        e' <- go e
        pure (EConstrainedValue e' constraints)
    go EGuardFail = pure EGuardFail

    -- A splice consumes one Q layer.  Do not recursively execute a value of
    -- type @Q (Q Exp)@ as though it were @Q Exp@.
    unwrapOneQ = runOneQExp

    elaborateQExp = elaborateAt env (TyApp (TyCon "Q") (TyCon "Exp"))

    goAlt (Alt p e) = Alt p <$> go e

    goStmt (SExpr e)         = SExpr <$> go e
    goStmt (SBind n e)       = SBind n <$> go e
    goStmt (SBangBind n e)   = SBangBind n <$> go e
    goStmt (SLet bs)         = SLet <$> mapM (\(n, b) -> (n,) <$> go b) bs
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
-- same encoding as source-loaded TH constructors produce.
exprToVal :: Expr -> IO Val
exprToVal (EVar n)
    -- Capitalised name → ConE; lowercase/operator → VarE.
    | not (BC.null n) && BC.head n >= 'A' && BC.head n <= 'Z' = do
        nt <- thName n
        force legacyHooks =<< newWHNFThunk (VCon "ConE" [nt])
    | otherwise = do
        nt <- thName n
        force legacyHooks =<< newWHNFThunk (VCon "VarE" [nt])
exprToVal (ELit (LInt n)) = do
    litT <- integerL n
    force legacyHooks =<< litE litT
exprToVal (ELit (LFloat d)) = do
    -- Encode floating-point as IntegerL (round) — RationalL needs more infra.
    litT <- integerL (round d)
    force legacyHooks =<< litE litT
exprToVal (ELit (LStr bs)) = do
    charListVal <- buildCharList (BC.unpack bs)
    litT <- stringL charListVal
    force legacyHooks =<< litE litT
  where
    buildCharList []     = pure (VCon "[]" [])
    buildCharList (c:cs) = do
        h    <- newWHNFThunk (VChar c)
        rest <- buildCharList cs
        t    <- newWHNFThunk rest
        pure (VCon ":" [h, t])
exprToVal (ELit (LChar c)) = do
    litT <- charL c
    force legacyHooks =<< litE litT
exprToVal (EApp f x) = do
    fV  <- exprToVal f
    xV  <- exprToVal x
    fT  <- newWHNFThunk fV
    xT  <- newWHNFThunk xV
    force legacyHooks =<< appE fT xT
exprToVal (ETuple es) = do
    elemVals <- mapM exprToVal es
    elemTs   <- mapM newWHNFThunk elemVals
    force legacyHooks =<< tupE elemTs
exprToVal (ENeg e) = do
    -- negate x  =  AppE (VarE "negate") x
    negT <- do
        nt <- thName "negate"
        newWHNFThunk (VCon "VarE" [nt])
    xV   <- exprToVal e
    xT   <- newWHNFThunk xV
    force legacyHooks =<< appE negT xT
-- Value-level @T: the type argument is opaque metadata. Encode the inner
-- expression as the TH Exp; a future splice pass that cares about type
-- applications can inspect the 'ETyApp' node before reaching here.
exprToVal (ETyApp e _ty) = exprToVal e
exprToVal (ELocalSig _ e) = exprToVal e
exprToVal (EConstrainedValue e _constraints) = exprToVal e
-- For other unsupported forms, emit a VarE "<unsupported>" placeholder.
exprToVal _ =
    force legacyHooks =<< newWHNFThunk (VCon "VarE" [])

--------------------------------------------------------------------------------
-- Builtin name pairs to register in the environment
--------------------------------------------------------------------------------

-- | No ordinary Template Haskell symbol is host-backed.
thBuiltinPairs :: [(ByteString, IO Val)]
thBuiltinPairs = []
-- Language.Haskell.TH.Syntax and Language.Haskell.TH.Lib have ordinary
-- Haskell source in template-haskell. Their Q/runQ/lift/name helpers, Loc
-- values, and TH AST constructors must therefore come from that source and
-- the normal constructor environment. Host support belongs only beneath the
-- source-defined Quasi IO methods; no such leaf is required by this module's
-- quote/splice decoder yet. Tests assert that this runtime surface stays empty.

--------------------------------------------------------------------------------
-- Phase 2.13: TH Dec -> IHC binding decoder
--------------------------------------------------------------------------------

-- | Evaluate a top-level splice expression and decode its result into
-- a list of @(name, body)@ top-level bindings.
thExpandSpliceDecl :: Env -> ImplicitParamMap -> Expr -> IO [(Name, Expr)]
thExpandSpliceDecl env ipm spliceExpr = do
    v0 <- eval legacyHooks env ipm spliceExpr
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
    hV <- force legacyHooks hT
    tV <- force legacyHooks tT
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
        patV <- force legacyHooks patT
        bodyV <- force legacyHooks bodyT
        name <- decodeVarP patV
        body <- decodeBody bodyV
        pure (Just (name, body))
    VCon "FunD" [nameT, clausesT] -> do
        nameV <- force legacyHooks nameT
        name <- decodeName nameV
        clausesV <- force legacyHooks clausesT
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
        nameV <- force legacyHooks nameT
        decodeName nameV
    decodeVarP v =
        throwTH ("thDecToBinding: ValD LHS must be VarP, got "
                 <> showValForDebug v)

    decodeBody (VCon "NormalB" [eT]) = do
        eV <- force legacyHooks eT
        thExpToExpr eV
    decodeBody (VCon "GuardedB" _) =
        throwTH "thDecToBinding: GuardedB (guards) not yet supported"
    decodeBody v =
        throwTH ("thDecToBinding: expected NormalB, got " <> showValForDebug v)

    decodeClauses (VCon "[]" []) =
        throwTH "thDecToBinding: FunD has no clauses"
    decodeClauses (VCon ":" [clT, restT]) = do
        clV <- force legacyHooks clT
        restV <- force legacyHooks restT
        case restV of
            VCon "[]" [] -> decodeOneClause clV
            _ -> throwTH "thDecToBinding: FunD with multiple clauses not yet supported"
    decodeClauses v =
        throwTH ("thDecToBinding: FunD clauses must be a list, got "
                 <> showValForDebug v)

    decodeOneClause (VCon "Clause" [patsT, bodyT, _decsT]) = do
        patsV <- force legacyHooks patsT
        bodyV <- force legacyHooks bodyT
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
        hV <- force legacyHooks hT
        tV <- force legacyHooks tT
        p  <- decodePat hV
        ps <- decodePats tV
        pure (p : ps)
    decodePats v =
        throwTH ("thDecToBinding: expected [Pat], got " <> showValForDebug v)

    decodePat (VCon "VarP" [nT]) = do
        nV <- force legacyHooks nT
        PVar <$> decodeName nV
    decodePat (VCon "WildP" []) = pure PWild
    decodePat (VCon "LitP" [lT]) = do
        lV <- force legacyHooks lT
        case lV of
            VCon "IntegerL" [iT] -> do
                iV <- force legacyHooks iT
                case iV of
                    VInt n -> pure (PLit (LInt n))
                    _ -> throwTH "LitP: IntegerL payload must be VInt"
            VCon "CharL" [cT] -> do
                cV <- force legacyHooks cT
                case cV of
                    VChar c -> pure (PLit (LChar c))
                    _ -> throwTH "LitP: CharL payload must be VChar"
            _ -> throwTH "LitP: unsupported literal"
    decodePat v =
        throwTH ("thDecToBinding: unsupported pattern " <> showValForDebug v)
