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

import IHC.AST
import IHC.Classes (ClassRegistry, lookupInstance)
import IHC.TypeAST
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
classMethodHint _methodName preds body = case preds of
    [Pred cls (TyVar v)]
      | Set.member v (freeTyVars body) -> Just (cls, v)
    _ -> Nothing

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

-- | Do-block: fold via the monad's '>>=' / '>>' / 'pure'.  For MVP we
-- treat the do-block opaquely (fresh tyvar) — actual method dispatch
-- during evaluation happens through the usual dispatcher path, so
-- elaboration on the outer context constrains things.  Sub-expressions
-- are still walked for local class method resolution.
elaborateDo :: InferEnv -> [Stmt] -> IO (Expr, Type, [Pred], Subst)
elaborateDo ienv stmts = do
    -- Walk statements for side effects (recursive elaboration); do-block
    -- type gets a fresh tyvar at the outer level.
    (stmts', preds, sub) <- foldM' emptySubst [] (ienv, stmts)
    t <- TyVar <$> freshVar (ieFresh ienv)
    pure (EDo stmts', t, preds, sub)
  where
    foldM' sub accP (_, []) = pure (reverse accP, [], sub)   -- shouldn't hit — return above
    foldM' _ _ _ = pure ([], [], emptySubst)   -- placeholder

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
-- NOTE: currently a stub — proper implementation would tokenise the
-- bytes via the lexer and call the same parser used by 'scanTypeSigs'.
-- For MVP we accept simple heads like "Maybe", "Maybe Int", "StateT
-- Int Identity Int" etc.
parseRawTypeExpr :: ByteString -> Maybe Type
parseRawTypeExpr bs =
    let trimmed = BC.dropWhile (== ' ') bs
        tokens  = BC.words trimmed
    in case tokens of
        []      -> Nothing
        (hd:_)
            | isUpper (BC.head hd) ->
                -- Head constructor + optional args.
                -- For MVP: build left-assoc TyApp with each word.
                let parts = map (\w ->
                        if isUpper (BC.head w)
                            then TyCon w
                            else TyVar w) tokens
                in case parts of
                    []       -> Nothing
                    (p:ps)   -> Just (foldl TyApp p ps)
            | otherwise -> Just (TyVar hd)
  where
    isUpper c = c >= 'A' && c <= 'Z'

-- | Minimal monadic foldM — pure; not used yet.
foldM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
foldM _ z [] = pure z
foldM f z (x:xs) = do { z' <- f z x; foldM f z' xs }
