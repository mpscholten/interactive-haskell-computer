-- | On-demand local type inference.  Invoked by the evaluator when
-- class dispatch hits an ambiguity: walks a sub-expression, unifies
-- against an expected type (or known-signature constraints), rewrites
-- ambiguous class method 'EVar's as 'ETypedMethod' nodes carrying
-- resolved instance tags.
--
-- MVP scope: rank-1 types, single-parameter class constraints,
-- let-polymorphism for local bindings, one-hop type synonym
-- expansion.  Higher-rank, GADTs, type families (beyond what
-- 'IHC.TypeReduce' already handles) are out.
--
-- Inference failures throw 'InferenceError' (an 'IhcException' at the
-- eval site) — silent fallbacks would hide real bugs.
module IHC.Elaborate
    ( InferenceError(..)
    , Expected(..)
    , elaborate
    , elaborateExpr
    , parseRawTypeExpr
    , resolveSynonymHop
    ) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import System.IO.Unsafe (unsafePerformIO)

import IHC.AST
import IHC.Classes (ClassRegistry, lookupInstance)
import IHC.TypeAST
import IHC.TypeGlobals (globalClassMethodNamesRef)
import IHC.TypeUnify

-- | Failure surfaced to the evaluator as an 'IhcException'.
data InferenceError
    = MissingSignature !Name
    | UnificationFailure !UnifyError
    | UnresolvedConstraint !Pred
    | UnknownClassInstance !Name !Name   -- class, resolved-head
    | AmbiguousDispatch !Name !Name       -- class, method
    | NotElaboratable !String
    deriving (Show)

instance Exception InferenceError

-- | What the caller expects about the elaboration root's type.
-- 'ExpectType' pins the root to a specific type (used for the
-- 'ETyApp' trigger).  'InferFreely' lets inference determine it.
data Expected
    = ExpectType !Type
    | InferFreely
    deriving (Show)

-- | Inference environment passed through the walker.  Not a record of
-- mutable state — pure-ish, though 'TypeSigs' and 'ClassReg' are read
-- via IORefs at the top level.
data InferEnv = InferEnv
    { ieFresh    :: !FreshSource
    , ieSigs     :: !(Map ByteString Scheme)
    , ieSynonyms :: !(Map ByteString (Int, Type))
    , ieClassReg :: !ClassRegistry
    , ieLocals   :: !(Map Name Scheme)   -- lambda-bound + let-bound
    }

-- | Top-level entry point.  Elaborate a sub-expression under an
-- expected type (or freely) given the global type signatures and
-- synonyms.  Returns the rewritten Expr (class method EVars swapped
-- for 'ETypedMethod' where inference resolved them) along with the
-- final substitution + inferred type.  Throws 'InferenceError' on
-- failure.
elaborate
    :: ClassRegistry
    -> Map ByteString Scheme
    -> Map ByteString (Int, Type)
    -> Expected
    -> Expr
    -> IO (Expr, Type)
elaborate classReg sigs synonyms expected e = do
    fresh <- newFreshSource
    let ienv = InferEnv
            { ieFresh    = fresh
            , ieSigs     = sigs
            , ieSynonyms = synonyms
            , ieClassReg = classReg
            , ieLocals   = Map.empty
            }
    (e', t, _preds, sub) <- elaborateExpr ienv e
    -- If we had an expected type, unify the result type.
    finalSub <- case expected of
        InferFreely     -> pure sub
        ExpectType want ->
            let wantResolved = expandSyn synonyms want
                tCur         = applySubst sub t
            in case unify sub wantResolved tCur of
                   Right s  -> pure s
                   Left ue  -> throwIO (UnificationFailure ue)
    let e''    = applyMethodSubst finalSub e'
        tFinal = applySubst finalSub t
    pure (e'', tFinal)

-- | Main inference walker.  Returns (rewritten Expr, inferred Type,
-- deferred constraints, substitution-so-far).  Constraints are
-- resolved (or deferred-to-unification) as we go; at the top level
-- any remaining unresolved predicates become 'UnresolvedConstraint'.
elaborateExpr :: InferEnv -> Expr -> IO (Expr, Type, [Pred], Subst)
elaborateExpr ienv expr = case expr of
    ELit (LInt _)   -> pure (expr, TyCon (BC.pack "Int"), [], emptySubst)
    ELit (LFloat _) -> pure (expr, TyCon (BC.pack "Double"), [], emptySubst)
    ELit (LChar _)  -> pure (expr, TyCon (BC.pack "Char"), [], emptySubst)
    ELit (LStr _)   ->
        -- [Char] — list of Char.
        pure ( expr
             , TyApp (TyCon (BC.pack "[]")) (TyCon (BC.pack "Char"))
             , []
             , emptySubst
             )

    EVar name -> elaborateVar ienv name

    EApp f x -> do
        (f', ft, fPreds, s1) <- elaborateExpr ienv f
        let ienvX = applySubstIenv s1 ienv
        (x', xt, xPreds, s2) <- elaborateExpr ienvX x
        resultTy <- TyVar <$> freshVar (ieFresh ienv)
        let fShould = TyArrow (applySubst s2 xt) resultTy
            fIs     = applySubst s2 (applySubst s1 ft)
        case unify (composeSubst s1 s2) fShould fIs of
            Left ue -> throwIO (UnificationFailure ue)
            Right s3 -> do
                let preds = map (applySubstPred s3) (fPreds ++ xPreds)
                pure ( EApp f' x'
                     , applySubst s3 resultTy
                     , preds
                     , s3
                     )

    ETyApp inner tyBytes -> do
        -- @e :: T@ and @e @T@ — pin inner's (result) type.
        case parseRawTypeExpr tyBytes of
            Nothing -> do
                -- Can't parse the type bytes; skip the constraint.
                (inner', t, preds, sub) <- elaborateExpr ienv inner
                pure (ETyApp inner' tyBytes, t, preds, sub)
            Just annotTy -> do
                let annotResolved = expandSyn (ieSynonyms ienv) annotTy
                (inner', innerT, preds, sub) <- elaborateExpr ienv inner
                case unify sub annotResolved innerT of
                    Left ue -> throwIO (UnificationFailure ue)
                    Right sub' ->
                        pure ( ETyApp inner' tyBytes
                             , applySubst sub' annotResolved
                             , map (applySubstPred sub') preds
                             , sub'
                             )

    ELam name body -> do
        argTy <- TyVar <$> freshVar (ieFresh ienv)
        let ienv' = ienv { ieLocals = Map.insert name (Scheme [] [] argTy)
                                                      (ieLocals ienv) }
        (body', bodyT, preds, sub) <- elaborateExpr ienv' body
        pure ( ELam name body'
             , TyArrow (applySubst sub argTy) bodyT
             , preds
             , sub
             )

    ELet bs body -> elaborateLet ienv bs body

    EDo stmts -> elaborateDo ienv stmts

    EIf c t e -> do
        (c', ct, pc, s1) <- elaborateExpr ienv c
        let ienv1 = applySubstIenv s1 ienv
        case unify s1 ct (TyCon (BC.pack "Bool")) of
            Left ue -> throwIO (UnificationFailure ue)
            Right s1' -> do
                (t', tt, pt, s2) <- elaborateExpr (applySubstIenv s1' ienv1) t
                let ienv2 = applySubstIenv s2 ienv1
                (e', et, pe, s3) <- elaborateExpr ienv2 e
                case unify s3 (applySubst s3 tt) (applySubst s3 et) of
                    Left ue -> throwIO (UnificationFailure ue)
                    Right sFinal -> do
                        let preds = map (applySubstPred sFinal) (pc ++ pt ++ pe)
                        pure ( EIf c' t' e'
                             , applySubst sFinal tt
                             , preds
                             , sFinal
                             )

    ETuple es -> do
        (es', ts, preds, sub) <- elaborateMany ienv es
        let n = length es
            tupleCon = TyCon (BC.pack ("(" ++ replicate (n - 1) ',' ++ ")"))
            tupleTy  = foldl TyApp tupleCon ts
        pure (ETuple es', tupleTy, preds, sub)

    ENeg e -> do
        (e', t, preds, sub) <- elaborateExpr ienv e
        pure (ENeg e', t, preds, sub)

    -- Already-resolved: pass-through.
    ETypedMethod cls method tag -> do
        fresh <- TyVar <$> freshVar (ieFresh ienv)
        pure (ETypedMethod cls method tag, fresh, [], emptySubst)

    -- Everything else: opaque to inference in this cut.
    -- We still walk sub-expressions for class method rewrites to bubble
    -- up, but the top-level type becomes a fresh var.
    _ -> do
        fresh <- TyVar <$> freshVar (ieFresh ienv)
        pure (expr, fresh, [], emptySubst)

-- | Handle an 'EVar' — look up signature, instantiate fresh vars,
-- collect class constraints.
--
-- Class-method detection: when a signature has exactly one class pred
-- whose argument is a single type variable that also appears in the
-- body type, treat the EVar as a class method dispatch site.  Emit an
-- 'ETypedMethod' node with the fresh tyvar as a placeholder tag —
-- 'applyMethodSubst' replaces the tag with the resolved head
-- constructor after unification.
elaborateVar :: InferEnv -> Name -> IO (Expr, Type, [Pred], Subst)
elaborateVar ienv name =
    case Map.lookup name (ieLocals ienv) of
        Just sch -> do
            (preds, ty) <- instantiate (ieFresh ienv) sch
            pure (EVar name, ty, preds, emptySubst)
        Nothing -> case Map.lookup name (ieSigs ienv) of
            Just sch -> do
                (preds, ty) <- instantiate (ieFresh ienv) sch
                case classMethodHint name preds ty of
                    Just (cls, paramVar) ->
                        -- Emit ETypedMethod with placeholder tag.
                        -- The tag is the fresh tyvar's name;
                        -- 'applyMethodSubst' resolves it later.
                        pure ( ETypedMethod cls name paramVar
                             , ty
                             , preds
                             , emptySubst
                             )
                    Nothing ->
                        pure (EVar name, ty, preds, emptySubst)
            Nothing ->
                -- No signature available.  Return a fresh tyvar — the
                -- enclosing context may still succeed if this var
                -- doesn't participate in class dispatch.
                do fresh <- TyVar <$> freshVar (ieFresh ienv)
                   pure (EVar name, fresh, [], emptySubst)

-- | If the signature has a single-parameter class constraint whose
-- argument is a plain type variable that also appears in the body
-- type, return @(className, tyVarName)@ — the info needed to emit
-- an 'ETypedMethod' for this var.  Otherwise 'Nothing'.
classMethodHint :: Name -> [Pred] -> Type -> Maybe (Name, Name)
classMethodHint methodName preds body = case preds of
    [Pred cls (TyVar v)]
      | Set.member v (freeTyVars body)
      , isActualClassMethod methodName ->
            Just (cls, v)
    _ -> Nothing

-- | Is @name@ actually declared as a method inside some @class C where
-- ... :: ...@ block?  Consulted so we don't route honest top-level
-- functions whose sigs happen to fit the "one class constraint whose
-- tyvar appears in the body" shape (@array :: Ix i => (i, i) -> [(i,
-- e)] -> Array i e@) through the class dispatcher.
--
-- 'unsafePerformIO' is safe because the referenced 'IORef' is
-- write-once-mostly (populated by the scheduler at program start) and
-- reads are commutative.
isActualClassMethod :: Name -> Bool
isActualClassMethod name =
    name `Set.member` unsafePerformIO (readIORef globalClassMethodNamesRef)

-- | Walk a list of expressions sequentially, threading substitution.
elaborateMany :: InferEnv -> [Expr] -> IO ([Expr], [Type], [Pred], Subst)
elaborateMany ienv = go emptySubst [] [] []
  where
    go sub accE accT accP [] =
        pure (reverse accE, reverse (map (applySubst sub) accT), accP, sub)
    go sub accE accT accP (e : es) = do
        (e', t, preds, s') <- elaborateExpr (applySubstIenv sub ienv) e
        let sub' = composeSubst sub s'
        go sub' (e' : accE) (t : accT) (preds ++ accP) es

-- | Let-binding: infer each binding's type (monomorphic for MVP —
-- let-polymorphism could be added later via 'generalize').
elaborateLet :: InferEnv -> [Bind] -> Expr -> IO (Expr, Type, [Pred], Subst)
elaborateLet ienv bs body = do
    -- Pre-seed each binding with a fresh tyvar for mutual recursion.
    preseed <- mapM (\(n, _) -> do
                        v <- freshVar (ieFresh ienv)
                        pure (n, TyVar v))
                    bs
    let ienvSeeded = ienv
            { ieLocals = foldr (\(n, t) m -> Map.insert n (Scheme [] [] t) m)
                               (ieLocals ienv) preseed
            }
    -- Infer each binding's body.
    (bs', preds, sub) <- inferBinds ienvSeeded emptySubst [] [] bs preseed
    let ienv' = (applySubstIenv sub ienvSeeded)
    (body', bodyT, bPreds, bSub) <- elaborateExpr ienv' body
    pure ( ELet bs' body'
         , bodyT
         , map (applySubstPred bSub) (preds ++ bPreds)
         , composeSubst sub bSub
         )
  where
    inferBinds _ sub accE accP [] _ = pure (reverse accE, accP, sub)
    inferBinds ie sub accE accP ((n, rhs) : rest) ((_, seeded) : preRest) = do
        (rhs', rhsT, preds, sub') <- elaborateExpr (applySubstIenv sub ie) rhs
        case unify sub' rhsT (applySubst sub' seeded) of
            Left ue -> throwIO (UnificationFailure ue)
            Right sub'' ->
                inferBinds ie (composeSubst sub sub'') ((n, rhs') : accE)
                           (preds ++ accP) rest preRest
    inferBinds _ _ _ _ _ _ = pure ([], [], emptySubst)

-- | Do-block: walk each statement and elaborate sub-expressions for
-- class-method rewrites.  The do-block's outer type stays a fresh
-- tyvar — a full bidirectional elaborator (pushing the enclosing
-- expected-type into each stmt to force @m@ in @m s -> s -> (a, s)@)
-- is out of MVP scope.  This partial pass still handles common cases
-- like @do { x <- getLine; putStrLn (... :: String) }@ where the
-- ambiguity is localised to a single sub-expression.
elaborateDo :: InferEnv -> [Stmt] -> IO (Expr, Type, [Pred], Subst)
elaborateDo ienv stmts = do
    (stmts', preds, sub) <- goStmts ienv emptySubst [] [] stmts
    t <- TyVar <$> freshVar (ieFresh ienv)
    pure (EDo stmts', t, preds, sub)
  where
    goStmts _ sub accS accP [] = pure (reverse accS, reverse accP, sub)
    goStmts ie sub accS accP (s : rest) = do
        (s', ie', preds, sub') <- goStmt ie sub s
        goStmts ie' sub' (s' : accS) (preds ++ accP) rest

    goStmt :: InferEnv -> Subst -> Stmt -> IO (Stmt, InferEnv, [Pred], Subst)
    goStmt ie sub stmt = case stmt of
        SExpr e -> do
            (e', _t, preds, s') <- elaborateExpr (applySubstIenv sub ie) e
            pure (SExpr e', ie, preds, composeSubst sub s')
        SBind name e -> do
            (e', _t, preds, s') <- elaborateExpr (applySubstIenv sub ie) e
            -- Add `name` to locals with a fresh tyvar — we don't
            -- unify the bind's result type with `m a` yet (MVP).
            fresh <- TyVar <$> freshVar (ieFresh ie)
            let ie' = ie { ieLocals = Map.insert name
                                          (Scheme [] [] fresh)
                                          (ieLocals ie) }
            pure (SBind name e', ie', preds, composeSubst sub s')
        SBangBind name e -> do
            -- Same shape as SBind; bang is a runtime-strictness annotation,
            -- not a type-level concern.
            (e', _t, preds, s') <- elaborateExpr (applySubstIenv sub ie) e
            fresh <- TyVar <$> freshVar (ieFresh ie)
            let ie' = ie { ieLocals = Map.insert name
                                          (Scheme [] [] fresh)
                                          (ieLocals ie) }
            pure (SBangBind name e', ie', preds, composeSubst sub s')
        SLet bs -> do
            -- Elaborate each binding's RHS; add names to locals.
            (bs', ie', preds, s') <- goLet (applySubstIenv sub ie) bs
            pure (SLet bs', ie', preds, composeSubst sub s')
        SImplicitLet bs -> do
            (bs', ie', preds, s') <- goLet (applySubstIenv sub ie) bs
            pure (SImplicitLet bs', ie', preds, composeSubst sub s')

    goLet :: InferEnv -> [(Name, Expr)] -> IO ([(Name, Expr)], InferEnv, [Pred], Subst)
    goLet ie bs = goL ie emptySubst [] [] bs
      where
        goL ie' sub accB accP [] = pure (reverse accB, ie', accP, sub)
        goL ie' sub accB accP ((n, rhs) : rest) = do
            (rhs', t, preds, s') <- elaborateExpr (applySubstIenv sub ie') rhs
            let ie'' = ie' { ieLocals = Map.insert n (Scheme [] [] t)
                                                     (ieLocals ie') }
            goL ie'' (composeSubst sub s') ((n, rhs') : accB)
                 (preds ++ accP) rest

-- | After unification, walk the expression replacing each
-- 'ETypedMethod's placeholder tag (a type-variable name) with the
-- resolved head constructor.  If the tag can't be resolved — the
-- class parameter is genuinely ambiguous after inference — we leave
-- the placeholder in place; the evaluator's 'ETypedMethod' case will
-- then throw a clear "no instance" error via
-- 'IHC.Eval.lookupInstanceMethod'.
applyMethodSubst :: Subst -> Expr -> Expr
applyMethodSubst sub = go
  where
    resolveTag :: Name -> Name
    resolveTag tag = case Map.lookup tag sub of
        Just ty -> case tyHead (applySubst sub ty) of
            Just h  -> h
            Nothing -> tag   -- still a tyvar or arrow; eval will error
        Nothing -> tag        -- tag is already a concrete head (direct rewrite)

    go e = case e of
        ETypedMethod cls method tag ->
            ETypedMethod cls method (resolveTag tag)
        EApp f x     -> EApp (go f) (go x)
        ELam n body  -> ELam n (go body)
        ELet bs body -> ELet [(n, go b) | (n, b) <- bs] (go body)
        ECase s as   -> ECase (go s) [Alt p (go e') | Alt p e' <- as]
        EIf c t b    -> EIf (go c) (go t) (go b)
        EDo stmts    -> EDo (map goStmt stmts)
        ENeg inner   -> ENeg (go inner)
        ETuple es    -> ETuple (map go es)
        ERecordCon n fs -> ERecordCon n [(nm, go v) | (nm, v) <- fs]
        ERecordUpdate inner fs ->
            ERecordUpdate (go inner) [(nm, go v) | (nm, v) <- fs]
        ESplice inner -> ESplice (go inner)
        EQuote inner  -> EQuote (go inner)
        ETyApp inner ty -> ETyApp (go inner) ty
        EImplicitLet bs body ->
            EImplicitLet [(n, go b) | (n, b) <- bs] (go body)
        _ -> e

    goStmt (SExpr e)         = SExpr (go e)
    goStmt (SBind n e)       = SBind n (go e)
    goStmt (SBangBind n e)   = SBangBind n (go e)
    goStmt (SLet bs)         = SLet [(n, go b) | (n, b) <- bs]
    goStmt (SImplicitLet bs) = SImplicitLet [(n, go b) | (n, b) <- bs]

-- | Apply a substitution to the local-var types in an inference env.
applySubstIenv :: Subst -> InferEnv -> InferEnv
applySubstIenv sub ie = ie
    { ieLocals = Map.map (applySubstScheme sub) (ieLocals ie) }

-- | One-hop type synonym expansion: @State s a@ → @StateT s Identity a@.
expandSyn :: Map ByteString (Int, Type) -> Type -> Type
expandSyn syns = go
  where
    go t = case t of
        TyApp _ _ ->
            let (head_, args) = tyApps t
                argsExpanded  = map go args
            in case head_ of
                TyCon n ->
                    case Map.lookup n syns of
                        Just (arity, rhs)
                          | length argsExpanded >= arity ->
                              let (forArity, extra) = splitAt arity argsExpanded
                                  subs = Map.fromList (zip (collectSynVars rhs arity)
                                                           forArity)
                                  expanded = applySubst subs rhs
                              in foldl TyApp expanded extra
                        _ -> foldl TyApp head_ argsExpanded
                _ -> foldl TyApp (go head_) argsExpanded
        TyCon _      -> t
        TyVar _      -> t
        TyArrow a b  -> TyArrow (go a) (go b)
        TyForall vs preds body -> TyForall vs preds (go body)

    -- Type synonym LHS variables aren't captured by the scanner right
    -- now (it only records arity + RHS).  For expansion we need names;
    -- we use conventional single-letter names 'a', 'b', …, matching
    -- the order the synonym scanner would have captured them.  This is
    -- a known shortcut; extending 'scanTypeSynonyms' to record LHS
    -- names would let this be exact.
    collectSynVars _ arity =
        take arity (map (BC.singleton . fst)
                        (zip ['a' ..] (repeat ())))

-- | One-hop synonym resolver.  Same as 'expandSyn' at the outer level
-- only — used by the trigger-finder to check if an annotation's head
-- is a synonym for something else.
resolveSynonymHop :: Map ByteString (Int, Type) -> Type -> Type
resolveSynonymHop = expandSyn

-- | Parse raw type-argument bytes (as stored in 'ETyApp') into a
-- 'Type'.  Shares the same grammar as 'IHC.Scan.parseScheme' body
-- parse.  Returns 'Nothing' on malformed input.
--
-- NOTE: still a partial parser — proper implementation would tokenise
-- the bytes via the lexer and call the same parser used by
-- 'scanTypeSigs'.  This hand-rolled variant handles the common
-- annotations actually seen at REPL prompts and inside fixtures:
-- bare constructors, applications, list brackets, parens, and
-- tuples.  Arrows and class contexts are not handled (they're rare
-- inside a @::@ on an expression).
parseRawTypeExpr :: ByteString -> Maybe Type
parseRawTypeExpr bs = do
    (t, rest) <- parseT (tokenize (trimSpaces bs))
    case dropSpacesT rest of
        [] -> Just t
        _  -> Just t   -- ignore trailing junk — best effort
  where
    trimSpaces = BC.dropWhile isSpc
    isSpc c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

    -- One simple type = list of atoms folded into a TyApp chain.
    parseT toks = do
        (a, rest) <- parseAtom toks
        (as, rest') <- parseAtoms rest
        pure (foldl TyApp a as, rest')

    parseAtoms toks = case dropSpacesT toks of
        [] -> Just ([], toks)
        (TRParen : _)   -> Just ([], toks)
        (TRBracket : _) -> Just ([], toks)
        (TComma : _)    -> Just ([], toks)
        _ -> case parseAtom toks of
            Nothing -> Just ([], toks)
            Just (a, rest) -> do
                (as, rest') <- parseAtoms rest
                pure (a : as, rest')

    parseAtom toks = case dropSpacesT toks of
        (TCon n : rest) -> Just (TyCon n, rest)
        (TVar n : rest) -> Just (TyVar n, rest)
        (TLBracket : rest) -> do
            (inner, rest1) <- parseT rest
            case dropSpacesT rest1 of
                (TRBracket : rest2) -> Just (TyApp (TyCon (BC.pack "[]")) inner, rest2)
                _ -> Nothing
        (TLParen : rest0) ->
            case dropSpacesT rest0 of
                (TRParen : rest1) ->
                    Just (TyCon (BC.pack "()"), rest1)
                _ -> do
                    (first, rest1) <- parseT rest0
                    case dropSpacesT rest1 of
                        (TRParen : rest2) -> Just (first, rest2)
                        (TComma : _)      -> parseTupleTail first rest1
                        _                 -> Nothing
        _ -> Nothing

    -- After first tuple element, parse remaining comma-separated
    -- elements and close with ')'.  Builds (,)-style tuple type.
    parseTupleTail first toks = do
        (elts, rest) <- goTup [first] toks
        let n        = length elts
            tupleCon = TyCon (BC.pack ("(" ++ replicate (n - 1) ',' ++ ")"))
        Just (foldl TyApp tupleCon elts, rest)
      where
        goTup acc toks' = case dropSpacesT toks' of
            (TComma : rest) -> do
                (t, rest1) <- parseT rest
                goTup (acc ++ [t]) rest1
            (TRParen : rest) -> Just (acc, rest)
            _ -> Nothing

    dropSpacesT :: [RTok] -> [RTok]
    dropSpacesT = id   -- tokenizer already strips whitespace

-- | Lightweight tokens for 'parseRawTypeExpr'.
data RTok
    = TCon !ByteString
    | TVar !ByteString
    | TLParen
    | TRParen
    | TLBracket
    | TRBracket
    | TComma
    deriving (Eq, Show)

tokenize :: ByteString -> [RTok]
tokenize bs
    | BC.null bs = []
    | otherwise =
        let c = BC.head bs
            rest = BC.tail bs
        in case c of
            ' '  -> tokenize rest
            '\t' -> tokenize rest
            '\n' -> tokenize rest
            '('  -> TLParen : tokenize rest
            ')'  -> TRParen : tokenize rest
            '['  -> TLBracket : tokenize rest
            ']'  -> TRBracket : tokenize rest
            ','  -> TComma : tokenize rest
            _
              | isIdentStart c ->
                    let (ident, rest') = BC.span isIdentChar bs
                        tok = if isUpper (BC.head ident)
                                then TCon ident
                                else TVar ident
                    in tok : tokenize rest'
              | otherwise ->
                    -- Unknown char: skip.  Prior behaviour was to
                    -- drop entire token stream; being permissive
                    -- works better for the common "trailing
                    -- whitespace / operator fragments" case.
                    tokenize rest
  where
    isIdentStart c = isUpper c || isLower c || c == '_'
    isIdentChar c  = isIdentStart c || (c >= '0' && c <= '9') || c == '\''
                     || c == '.'
    isUpper c = c >= 'A' && c <= 'Z'
    isLower c = c >= 'a' && c <= 'z'

-- | Minimal monadic foldM — pure; not used yet.
foldM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldM _ z [] = pure z
foldM f z (x:xs) = do { z' <- f z x; foldM f z' xs }
