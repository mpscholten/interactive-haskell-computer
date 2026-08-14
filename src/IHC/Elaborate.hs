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
    , elaborateWithScopedSigs
    , lookupScopedScheme
    , elaborateOwned
    , elaborateOwnedMethod
    , elaborateOwnedWithScopedSigs
    , elaborateExpr
    , parseRawTypeExpr
    , resolveSynonymHop
    ) where

import Control.Exception (Exception, SomeException, throwIO, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set


import IHC.AST
import IHC.Classes (ClassRegistry)
import IHC.ConstructorMetadata
    ( ConstructorTypeRegistry, constructorScheme )
import IHC.StringUtils (isAsciiSpace)
import IHC.TypeAST
import IHC.TypeGlobals (globalClassMethodNamesRef, globalAmbiguousSigsRef)
import IHC.TypeSchemeParser (parseSchemeBytes)
import IHC.TypeUnify

-- | Look up a scheme for expected-argument inference without consulting a
-- process-global last-writer entry when lexical metadata could not resolve an
-- ambiguous bare name.
lookupScopedScheme
    :: Set.Set ByteString
    -> Set.Set ByteString
    -> Map ByteString Scheme
    -> Name
    -> Maybe Scheme
lookupScopedScheme ambiguous scoped sigs name
    | Set.member bare ambiguous
    , not (Set.member name scoped || Set.member bare scoped) = Nothing
    | otherwise = Map.lookup name sigs `orElse` Map.lookup bare sigs
  where
    bare = bareName name
    orElse (Just value) _ = Just value
    orElse Nothing other = other

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
    , ieSynonyms :: !(Map ByteString TypeSynonym)
    , ieClassReg :: !ClassRegistry
    , ieLocals   :: !(Map Name Scheme)   -- lambda-bound + let-bound
    , ieClassMethodNames :: !(Set.Set ByteString)
    , ieAmbiguousSigs    :: !(Set.Set ByteString)
    , ieScopedSigs       :: !(Set.Set ByteString)
    , ieConstructorTypes :: !ConstructorTypeRegistry
    , ieOwner            :: !(Maybe Name)
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
    -> Map ByteString TypeSynonym
    -> Expected
    -> Expr
    -> IO (Expr, Type)
elaborate classReg sigs synonyms =
    elaborateOwnedWithScopedSigs classReg sigs synonyms Map.empty Nothing Set.empty

-- | Elaborate with signatures resolved in a concrete lexical owner.  Names in
-- this set are safe even when the process-global bare-name table records a
-- collision: their schemes came from the owner's actual import scope.
elaborateWithScopedSigs
    :: ClassRegistry
    -> Map ByteString Scheme
    -> Map ByteString TypeSynonym
    -> Set.Set ByteString
    -> Expected
    -> Expr
    -> IO (Expr, Type)
elaborateWithScopedSigs classReg sigs synonyms scopedSigs expected e = do
    elaborateOwnedWithScopedSigs classReg sigs synonyms Map.empty Nothing scopedSigs expected e

elaborateOwned
    :: ClassRegistry
    -> Map ByteString Scheme
    -> Map ByteString TypeSynonym
    -> ConstructorTypeRegistry
    -> Maybe Name
    -> Expected
    -> Expr
    -> IO (Expr, Type)
elaborateOwned classReg sigs synonyms constructorTypes owner expected e = do
    elaborateOwnedWithScopedSigs classReg sigs synonyms constructorTypes owner Set.empty expected e

-- | Elaborate with both owner-qualified constructor metadata and signatures
-- proven to come from the owner's lexical scope.  Keeping these inputs
-- together prevents either constructor recovery or ambiguity hardening from
-- being lost when expected types flow across partial applications.
elaborateOwnedWithScopedSigs
    :: ClassRegistry
    -> Map ByteString Scheme
    -> Map ByteString TypeSynonym
    -> ConstructorTypeRegistry
    -> Maybe Name
    -> Set.Set ByteString
    -> Expected
    -> Expr
    -> IO (Expr, Type)
elaborateOwnedWithScopedSigs classReg sigs synonyms constructorTypes owner scopedSigs expected e = do
    elaborateOwnedInternal False classReg sigs synonyms constructorTypes owner scopedSigs expected e

elaborateOwnedMethod
    :: ClassRegistry -> Map ByteString Scheme -> Map ByteString TypeSynonym
    -> ConstructorTypeRegistry -> Maybe Name -> Expected -> Expr
    -> IO (Expr, Type)
elaborateOwnedMethod classReg sigs synonyms constructorTypes owner expected e =
    elaborateOwnedInternal True classReg sigs synonyms constructorTypes owner Set.empty expected e

elaborateOwnedInternal
    :: Bool -> ClassRegistry -> Map ByteString Scheme
    -> Map ByteString TypeSynonym -> ConstructorTypeRegistry -> Maybe Name
    -> Set.Set ByteString -> Expected -> Expr -> IO (Expr, Type)
elaborateOwnedInternal _seedMethodBinders classReg sigs synonyms constructorTypes owner scopedSigs expected e = do
    fresh <- newFreshSource
    classMethodNames <- readIORef globalClassMethodNamesRef
    ambiguousSigs    <- readIORef globalAmbiguousSigsRef
    let ienv = InferEnv
            { ieFresh    = fresh
            , ieSigs     = sigs
            , ieSynonyms = synonyms
            , ieClassReg = classReg
            , ieLocals   = Map.empty
            , ieClassMethodNames = classMethodNames
            , ieAmbiguousSigs    = ambiguousSigs
            , ieScopedSigs       = scopedSigs
            , ieConstructorTypes = constructorTypes
            , ieOwner            = owner
            }
    (e', t, _preds, sub) <- case expected of
        ExpectType want -> elaborateExpectedExpr ienv (expandSyn synonyms want) e
        InferFreely -> elaborateExpr ienv e
    -- If we had an expected type, unify the result type.
    finalSub <- case expected of
        InferFreely     -> pure sub
        ExpectType want ->
            let wantResolved = expandSyn synonyms want
                tCur         = applySubst sub t
            in case unify sub wantResolved tCur of
                   Right s  -> pure s
                   Left ue  -> throwIO (UnificationFailure ue)
    let e''    = applyMethodSubst synonyms finalSub e'
        tFinal = applySubst finalSub t
    pure (e'', tFinal)

elaborateExpectedExpr :: InferEnv -> Type -> Expr
    -> IO (Expr, Type, [Pred], Subst)
elaborateExpectedExpr ienv expected expr = case (expr, expected) of
    -- OverloadedStrings: a string literal (or its desugared cons list)
    -- whose expected type is not [Char]/String is `fromString` at that
    -- result type.  The class method is result-polymorphic; the
    -- expected type (constructor field, annotation) chooses the
    -- instance.  No library or constructor name is special-cased.
    (e, ty)
      | not (isCharListType ty)
      , isStringLiteralExpr e ->
            elaborateExpectedExpr ienv ty
                (EApp (EVar (BC.pack "fromString")) e)
    (EVar name, _)
      | Just (Scheme vars preds _) <- Map.lookup name (ieLocals ienv)
      , not (null vars) || not (null preds)
      , Just expectedBytes <- renderExpectedType expected -> do
        (_, actual, actualPreds, sub) <- elaborateVar ienv name
        sub' <- either (throwIO . UnificationFailure) pure
            (unify sub expected actual)
        pure (ETyApp (EVar name) expectedBytes, applySubst sub' expected,
            map (applySubstPred sub') actualPreds, sub')
    (ELam name body, TyArrow argTy resultTy) -> do
        let ie = ienv { ieLocals = Map.insert name (Scheme [] [] argTy)
                                    (ieLocals ienv) }
        (body', bodyTy, preds, sub) <- elaborateExpectedExpr ie resultTy body
        sub' <- either (throwIO . UnificationFailure) pure
            (unify sub (applySubst sub resultTy) (applySubst sub bodyTy))
        pure (ELam name body', TyArrow (applySubst sub' argTy)
            (applySubst sub' bodyTy), map (applySubstPred sub') preds, sub')
    (ELet bs body, _) -> elaborateLetExpected ienv bs expected body
    (ECase scrut alts, _) -> do
        (scrut', scrutTy, scrutPreds, scrutSub) <- elaborateExpr ienv scrut
        (alts', altPreds, finalSub) <- elaborateExpectedAlts
            (applySubstIenv scrutSub ienv) (applySubst scrutSub scrutTy)
            expected scrutSub alts
        pure (ECase scrut' alts', applySubst finalSub expected,
            map (applySubstPred finalSub) (scrutPreds ++ altPreds), finalSub)
    _ -> elaborateExpr ienv expr

-- | [Char] after synonym expansion.  String is expanded by the
-- caller via 'expandSyn' before 'elaborateExpectedExpr'.
isCharListType :: Type -> Bool
isCharListType (TyApp (TyCon n) (TyCon e)) =
    (n == BC.pack "[]" || n == BC.pack "List") && e == BC.pack "Char"
isCharListType (TyCon n) = n == BC.pack "String"
isCharListType _ = False

-- | Parser-desugared @"…"@ is a cons-chain of 'ELit' 'LChar', or the
-- original 'LStr' if desugar has not run.
isStringLiteralExpr :: Expr -> Bool
isStringLiteralExpr (ELit (LStr _)) = True
isStringLiteralExpr (ELit (LChar _)) = False
isStringLiteralExpr (EVar n) = n == BC.pack "[]"
isStringLiteralExpr (EApp (EApp (EVar cons) (ELit (LChar _))) rest)
    | cons == BC.pack ":" || cons == BC.pack "GHC.Types.:" =
        isStringLiteralExpr rest
isStringLiteralExpr (EApp (EApp (EVar cons) (ELit (LChar _))) rest)
    | BC.isSuffixOf (BC.pack ".:") cons = isStringLiteralExpr rest
isStringLiteralExpr _ = False

renderExpectedType :: Type -> Maybe ByteString
renderExpectedType ty = case ty of
    TyVar n -> Just n
    TyCon n -> Just n
    TyApp f x -> do
        fb <- renderExpectedType f
        xb <- atom x
        pure (BC.concat [fb, BC.singleton ' ', xb])
    TyArrow a b -> do
        ab <- atom a
        bb <- renderExpectedType b
        pure (BC.concat [ab, BC.pack " -> ", bb])
    TyForall{} -> Nothing
  where
    atom t@TyApp{} = parens t
    atom t@TyArrow{} = parens t
    atom t = renderExpectedType t
    parens t = do
        inner <- renderExpectedType t
        pure (BC.concat [BC.singleton '(', inner, BC.singleton ')'])

elaborateExpectedAlts :: InferEnv -> Type -> Type -> Subst -> [Alt]
    -> IO ([Alt], [Pred], Subst)
elaborateExpectedAlts _ _ _ sub [] = pure ([], [], sub)
elaborateExpectedAlts ienv scrutTy expected sub (Alt pat rhs : rest) = do
    (patLocals, patSub) <- inferPatternLocals (applySubstIenv sub ienv)
        (applySubst sub scrutTy) pat
    let sub1 = composeSubst sub patSub
        baseEnv = applySubstIenv sub1 ienv
        rhsEnv = baseEnv { ieLocals = Map.union patLocals (ieLocals baseEnv) }
    (rhs', rhsTy, rhsPreds, rhsSub) <- elaborateExpectedExpr
        rhsEnv (applySubst sub1 expected) rhs
    sub' <- either (throwIO . UnificationFailure) pure
        (unify (composeSubst sub1 rhsSub) (applySubst rhsSub expected) rhsTy)
    (rest', restPreds, finalSub) <- elaborateExpectedAlts
        (applySubstIenv sub' ienv) (applySubst sub' scrutTy)
        (applySubst sub' expected) sub' rest
    pure (Alt pat rhs' : rest', rhsPreds ++ restPreds, finalSub)

inferPatternLocals :: InferEnv -> Type -> Pat -> IO (Map Name Scheme, Subst)
inferPatternLocals ienv expected pat = case pat of
    PVar name -> pure (Map.singleton name (typeAsScheme expected), emptySubst)
    PWild -> pure (Map.empty, emptySubst)
    PBang inner -> inferPatternLocals ienv expected inner
    PIrref inner -> inferPatternLocals ienv expected inner
    PAs name inner -> do
        (locals, sub) <- inferPatternLocals ienv expected inner
        pure (Map.insert name (typeAsScheme (applySubst sub expected)) locals, sub)
    PCon ctor fields -> inferConstructorFields ctor fields
    PTuple fields -> inferConstructorFields
        (BC.pack ("(" ++ replicate (length fields - 1) ',' ++ ")")) fields
    PRecord{} -> pure (Map.empty, emptySubst)
    PRecordWild{} -> pure (Map.empty, emptySubst)
    _ -> pure (Map.empty, emptySubst)
  where
    inferConstructorFields ctor fields = case constructorScheme
            (ieConstructorTypes ienv) (ieOwner ienv) ctor of
        Nothing -> pure (Map.empty, emptySubst)
        Just scheme -> do
            (preds, ctorTy) <- instantiate (ieFresh ienv) scheme
            let (fieldTys, resultTy) = tyArrowArgs ctorTy
            if length fieldTys /= length fields || not (null preds)
                then pure (Map.empty, emptySubst)
                else case unify emptySubst resultTy expected of
                    Left _ -> pure (Map.empty, emptySubst)
                    Right resultSub -> foldFields resultSub Map.empty
                        (zip fields fieldTys)
    foldFields sub locals [] = pure (locals, sub)
    foldFields sub locals ((fieldPat, fieldTy) : fields) = do
        let concrete = applySubst sub fieldTy
        (newLocals, fieldSub) <- inferPatternLocals
            (applySubstIenv sub ienv) concrete fieldPat
        let sub' = composeSubst sub fieldSub
        foldFields sub' (Map.union newLocals locals) fields
    typeAsScheme (TyForall vars preds body) = Scheme vars preds body
    typeAsScheme ty = Scheme [] [] ty

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

    ELocalSig schemeBytes inner ->
        case parseSchemeBytes schemeBytes of
            Nothing -> do
                (inner', t, preds, sub) <- elaborateExpr ienv inner
                pure (ELocalSig schemeBytes inner', t, preds, sub)
            Just declaredScheme -> do
                (declaredPreds, declaredTy) <- instantiate
                    (ieFresh ienv) (expandScheme (ieSynonyms ienv) declaredScheme)
                let expected = declaredTy
                (inner', innerTy, preds, sub) <- elaborateExpectedExpr ienv expected inner
                sub' <- either (throwIO . UnificationFailure) pure
                    (unify sub expected innerTy)
                pure (ELocalSig schemeBytes inner', applySubst sub' expected,
                      map (applySubstPred sub') (declaredPreds ++ preds), sub')

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
            (preds, ty) <- instantiate (ieFresh ienv)
                (expandScheme (ieSynonyms ienv) sch)
            pure (EVar name, ty, preds, emptySubst)
        Nothing -> do
            mSig <- case builtinConstructorSig (bareName name) of
                Just sch -> pure (Just sch)
                Nothing -> case constructorScheme
                    (ieConstructorTypes ienv) (ieOwner ienv) name of
                        Just sch -> pure (Just sch)
                        Nothing  -> lookupSig
            case mSig of
                Just sch -> do
                    (preds, ty) <- instantiate (ieFresh ienv)
                        (expandScheme (ieSynonyms ienv) sch)
                    -- Use the BARE (unqualified) name for class-method detection
                    -- and the emitted node.  In a non-entry module the binding's
                    -- RHS has had its free vars import-rewritten to FQNs (e.g.
                    -- @GHC.Enum.minBound@, @Data.Array.Base.listArray@), but the
                    -- sig table and 'globalClassMethodNamesRef' are bare-keyed, and
                    -- an 'ETypedMethod's method is looked up by bare name at eval
                    -- time.  Without this, signature-directed elaboration fired
                    -- only for the entry module (where names stay bare) — e.g.
                    -- http-types' @methodArray@ resolved its @(minBound,maxBound)@
                    -- bounds in a single-file repro but defaulted to Int when
                    -- imported (the warp request path), surfacing as
                    -- @Ix Int.index: non-Int index@.
                    let bare = bareName name
                    case classMethodHint ienv bare preds ty of
                        Just (cls, paramVar) ->
                            -- Emit ETypedMethod with placeholder tag (bare method
                            -- name).  The tag is the fresh tyvar's name;
                            -- 'applyMethodSubst' resolves it later.
                            pure ( ETypedMethod cls bare paramVar
                                 , ty
                                 , preds
                                 , emptySubst
                                 )
                        Nothing ->
                            let constrained = constrainedValueHint preds ty
                                valueExpr = case constrained of
                                    [constraint] -> EConstrainedValue (EVar name) [constraint]
                                    -- The evaluator carries one dictionary key
                                    -- on a constrained value.  Multiple class
                                    -- predicates need real dictionary passing;
                                    -- silently keeping only one is unsound.
                                    _ -> EVar name
                            in pure (valueExpr, ty, preds, emptySubst)
                Nothing ->
                    -- No signature available.  Return a fresh tyvar — the
                    -- enclosing context may still succeed if this var
                    -- doesn't participate in class dispatch.
                    do fresh <- TyVar <$> freshVar (ieFresh ienv)
                       pure (EVar name, fresh, [], emptySubst)
  where
    -- GHC.Types has no Haskell source, so its list constructors are genuine
    -- compiler-built values.  Their schemes let expected-type elaboration
    -- recover the type of parser-desugared string literals (chains of ':' /
    -- '[]') without depending on a source-module signature having loaded.
    builtinConstructorSig n
        | n == BC.pack "[]" =
            Just (Scheme [BC.pack "a"] []
                (TyApp (TyCon (BC.pack "[]")) (TyVar (BC.pack "a"))))
        | n == BC.pack ":" =
            let a = TyVar (BC.pack "a")
                listA = TyApp (TyCon (BC.pack "[]")) a
            in Just (Scheme [BC.pack "a"] []
                (TyArrow a (TyArrow listA listA)))
        -- Function composition.  Category (.) has type
        --   cat b c -> cat a b -> cat a c
        -- which does not unify with TyArrow, so a result-polymorphic
        -- method in `toEnum . f` stays unconstrained and value-directed
        -- dispatch defaults it to Enum Int (the identity).  The
        -- function-arrow scheme lets the expected result type flow into
        -- the first argument, matching runtime baseDot.
        --
        -- Non-entry modules rewrite @.@ to a FQN such as
        -- @GHC.Internal.Base..@; bareName leaves that spelling intact
        -- (the last '.' is the operator), so accept the @..@ suffix.
        | n == BC.pack "." || BC.isSuffixOf (BC.pack "..") n =
            let a = TyVar (BC.pack "a")
                b = TyVar (BC.pack "b")
                c = TyVar (BC.pack "c")
            in Just (Scheme [BC.pack "a", BC.pack "b", BC.pack "c"] []
                (TyArrow (TyArrow b c)
                    (TyArrow (TyArrow a b) (TyArrow a c))))
        | otherwise = Nothing

    -- Try the name as written, then its bare last component, so FQNs from a
    -- non-entry module's import-rewritten RHS still find the bare-keyed sig.
    -- BUT decline if the bare name is AMBIGUOUS (conflicting sigs across
    -- loaded modules, e.g. @map@ once @Data.List.NonEmpty@ is in scope): the
    -- flat table holds only the last-writer's scheme, so using it would unify
    -- against the wrong shape (@NonEmpty a@ vs @[]@) and abort the whole
    -- rewrite.  Treating it as opaque (Nothing → fresh tyvar) is safe — a
    -- non-class-method like @map@ doesn't need a sig for the surrounding
    -- signature-directed resolution to succeed.
    lookupSig = pure $ case Map.lookup name (ieSigs ienv) of
        -- Only an owner-qualified spelling or a scheme explicitly proven to
        -- come from the owner's lexical scope is authoritative when the bare
        -- spelling collides.  The process-global table is also keyed by bare
        -- names, so treating every exact hit as scoped would merely select its
        -- last writer.
        Just s
            | isQualifiedName name
           || not (isAmbiguousSig ienv (bareName name)) -> Just s
            -- Ambiguous last-writer is a red herring for result-
            -- polymorphic class methods: the class scheme is unique
            -- and the expected type picks the instance.
            | isActualClassMethod ienv (bareName name)
            , schemeIsResultPolymorphic s -> Just s
            | otherwise -> Nothing
        Nothing
            | isAmbiguousSig ienv (bareName name)
            , isActualClassMethod ienv (bareName name)
            , Just s <- Map.lookup (bareName name) (ieSigs ienv)
            , schemeIsResultPolymorphic s -> Just s
            | isAmbiguousSig ienv (bareName name) -> Nothing
            | otherwise -> Map.lookup (bareName name) (ieSigs ienv)

    isQualifiedName n = case BC.elemIndexEnd '.' n of
        Just i -> i > 0 && i + 1 < BC.length n
        Nothing -> False

-- | Strip a module qualifier: @GHC.Enum.minBound@ → @minBound@, @minBound@ → @minBound@.
bareName :: Name -> Name
bareName n = case BC.elemIndexEnd '.' n of
    Just i | i + 1 < BC.length n -> BC.drop (i + 1) n
    _ -> n

-- | If the signature has a single-parameter class constraint whose
-- argument is a plain type variable that also appears in the body
-- type, return @(className, tyVarName)@ — the info needed to emit
-- an 'ETypedMethod' for this var.  Otherwise 'Nothing'.
classMethodHint :: InferEnv -> Name -> [Pred] -> Type -> Maybe (Name, Name)
classMethodHint ienv methodName preds body = case preds of
    [Pred cls [TyVar v]]
      | Set.member v (freeTyVars body)
      , isActualClassMethod ienv methodName ->
            Just (cls, v)
    _ -> Nothing

-- | Preserve the complete class-instance key for constrained values and
-- aliases.  Unlike 'classMethodHint', this is not limited to actual class
-- methods: a constrained alias can carry a multi-parameter predicate even
-- though its value ultimately evaluates to the real method dispatcher.
constrainedValueHint :: [Pred] -> Type -> [(Name, [Name])]
constrainedValueHint preds body = mapMaybe one preds
  where
    bodyVars = freeTyVars body
    one (Pred cls args)
        | all isPlainArg args
        , any (not . Set.null . Set.intersection bodyVars . freeTyVars) args =
            Just (cls, map argName args)
        | otherwise = Nothing
    one QPred{} = Nothing
    isPlainArg TyVar{} = True
    isPlainArg TyCon{} = True
    isPlainArg _       = False
    argName (TyVar n) = n
    argName (TyCon n) = n
    argName _         = BC.empty

-- | Is @name@ actually declared as a method inside some @class C where
-- ... :: ...@ block?  Consulted so we don't route honest top-level
-- functions whose sigs happen to fit the "one class constraint whose
-- tyvar appears in the body" shape (@array :: Ix i => (i, i) -> [(i,
-- e)] -> Array i e@) through the class dispatcher.
--
isActualClassMethod :: InferEnv -> Name -> Bool
isActualClassMethod ienv name =
    Set.member name (ieClassMethodNames ienv)

-- | Does this bare name have CONFLICTING signatures across the loaded modules
-- (so the flat 'globalTypeSigsRef' entry is whichever module loaded last)?
-- See 'IHC.Scheduler.mirrorTypeSigsGlobal' / 'globalAmbiguousSigsRef'.  Same
isAmbiguousSig :: InferEnv -> Name -> Bool
isAmbiguousSig ienv name =
    Set.member name (ieAmbiguousSigs ienv)
        && not (Set.member name (ieScopedSigs ienv))

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

-- | Let-binding: explicit local signatures provide rank-1 polymorphism;
-- unsigned recursive bindings keep their existing monomorphic seed.
elaborateLet :: InferEnv -> [Bind] -> Expr -> IO (Expr, Type, [Pred], Subst)
elaborateLet ienv bs body = do
    -- An explicit local signature is the binding's lexical scheme, including
    -- its forall binders and context.  Unsigned bindings retain the existing
    -- monomorphic recursion seed.  In particular, never consult the flat
    -- process-global signature table for a signed local that shadows it.
    preseed <- mapM seedBinding bs
    let ienvSeeded = ienv
            { ieLocals = foldr (\(n, sch) m -> Map.insert n sch m)
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
    seedBinding (n, rhs) = case bindingScheme rhs of
        Just sch -> pure (n, expandScheme (ieSynonyms ienv) sch)
        Nothing -> do
            v <- freshVar (ieFresh ienv)
            pure (n, Scheme [] [] (TyVar v))

    bindingScheme (ELocalSig raw _) = parseSchemeBytes raw
    bindingScheme _ = Nothing

    inferBinds _ sub accE accP [] _ = pure (reverse accE, accP, sub)
    inferBinds ie sub accE accP ((n, rhs) : rest) ((_, seededScheme) : preRest) = do
        (_seedPreds, seeded) <- instantiate (ieFresh ie) seededScheme
        (rhs', rhsT, preds, sub') <- elaborateExpr (applySubstIenv sub ie) rhs
        case unify sub' rhsT (applySubst sub' seeded) of
            Left ue -> throwIO (UnificationFailure ue)
            Right sub'' ->
                inferBinds ie (composeSubst sub sub'') ((n, rhs') : accE)
                           (preds ++ accP) rest preRest
    inferBinds _ _ _ _ _ _ = pure ([], [], emptySubst)

elaborateLetExpected :: InferEnv -> [Bind] -> Type -> Expr
    -> IO (Expr, Type, [Pred], Subst)
elaborateLetExpected ienv bs expected body = do
    preseed <- mapM (\(n, _) -> do
                        v <- freshVar (ieFresh ienv)
                        pure (n, TyVar v)) bs
    let seededEnv = ienv
            { ieLocals = foldr (\(n, t) m -> Map.insert n (Scheme [] [] t) m)
                               (ieLocals ienv) preseed }
    (bs', bindPreds, bindSub) <- inferBinds seededEnv emptySubst [] [] bs preseed
    let bodyEnv = applySubstIenv bindSub seededEnv
        bodyExpected = applySubst bindSub expected
    (body', bodyTy, bodyPreds, bodySub) <- elaborateExpectedExpr
        bodyEnv bodyExpected body
    finalSub <- either (throwIO . UnificationFailure) pure
        (unify (composeSubst bindSub bodySub)
            (applySubst bodySub bodyExpected) bodyTy)
    pure (ELet bs' body', applySubst finalSub expected,
        map (applySubstPred finalSub) (bindPreds ++ bodyPreds), finalSub)
  where
    inferBinds _ sub accE accP [] _ = pure (reverse accE, accP, sub)
    inferBinds ie sub accE accP ((n, rhs) : rest) ((_, seeded) : preRest) = do
        inferred <- try (elaborateExpr (applySubstIenv sub ie) rhs)
            :: IO (Either SomeException (Expr, Type, [Pred], Subst))
        case inferred of
            Left _ -> inferBinds ie sub ((n, rhs) : accE) accP rest preRest
            Right (rhs', rhsT, preds, rhsSub) ->
                case unify rhsSub rhsT (applySubst rhsSub seeded) of
                    Left _ -> inferBinds ie sub ((n, rhs) : accE) accP rest preRest
                    Right unified ->
                        inferBinds ie (composeSubst sub unified) ((n, rhs') : accE)
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
applyMethodSubst :: Map ByteString TypeSynonym -> Subst -> Expr -> Expr
applyMethodSubst synonyms sub = go
  where
    -- Resolve a placeholder tag to its concrete head constructor.  Returns
    -- 'Nothing' when the class parameter is still ambiguous after inference
    -- (the tag is a type variable, not a real type head) — the caller then
    -- reverts the node to a bare 'EVar' (see 'go').
    resolveTag :: Name -> Maybe Name
    resolveTag tag = case Map.lookup tag sub of
        Just ty ->
            let resolved = expandTypeSynonyms synonyms (applySubst sub ty)
            in if Set.null (freeTyVars resolved)
                   then Just (typeDispatchTag resolved)
                   else Nothing
        Nothing
          | isHeadName tag -> Just (typeDispatchTag (expandTypeSynonyms synonyms (TyCon tag)))
          | otherwise      -> Nothing            -- unresolved placeholder type variable

    -- A resolved type head is a constructor: an uppercase name, or the list /
    -- tuple constructors.  Type-variable placeholders (lowercase, or the
    -- fresh-var @$t…@ names) are not heads.
    isHeadName t = case BC.uncons t of
        Just (c, _) -> (c >= 'A' && c <= 'Z') || c == '[' || c == '('
        Nothing     -> False

    go e = case e of
        ETypedMethod cls method tag ->
            case resolveTag tag of
                Just h  -> ETypedMethod cls method h
                -- Tag stayed ambiguous (a type variable).  Revert to the bare
                -- name rather than emitting a broken 'ETypedMethod' whose tag
                -- has no instance: that node would (a) make the eval-site's
                -- 'allTypedMethodsResolvable' reject the WHOLE rewrite — losing
                -- the siblings that DID resolve (e.g. listArray's
                -- @(minBound,maxBound)@ bounds while an element-list @maxBound@
                -- stayed ambiguous) — and (b) error at eval.  A bare 'EVar'
                -- falls to runtime value-directed dispatch / defaults instead.
                Nothing -> EVar method
        EConstrainedValue inner constraints ->
            case traverse resolveConstraint constraints of
                Just resolved -> EConstrainedValue (go inner) resolved
                Nothing       -> go inner
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
        ELocalSig ty inner -> ELocalSig ty (go inner)
        EImplicitLet bs body ->
            EImplicitLet [(n, go b) | (n, b) <- bs] (go body)
        _ -> e

    resolveConstraint (cls, tags) = do
        resolved <- traverse resolveLegacyTag tags
        pure (cls, resolved)

    -- Constrained aliases are consumed by the value-directed MPTC dispatcher,
    -- with one tag per class parameter.  Preserve a parameter's complete
    -- concrete type in that slot: reducing @[Char]@ and @[Markup]@ to the same
    -- outer @[]@ tag loses information when an alias delegates to a method.
    -- Partially resolved parameters retain the older outer-head fallback.
    resolveLegacyTag tag = case Map.lookup tag sub of
        Just ty ->
            let resolved = expandTypeSynonyms synonyms (applySubst sub ty)
            in if Set.null (freeTyVars resolved)
                   then Just (typeDispatchTag resolved)
                   else tyHead resolved
        Nothing
          | isHeadName tag -> Just (typeDispatchTag (expandTypeSynonyms synonyms (TyCon tag)))
          | otherwise -> Nothing

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
expandSyn :: Map ByteString TypeSynonym -> Type -> Type
expandSyn syns = go
  where
    go t = case t of
        TyApp _ _ ->
            let (head_, args) = tyApps t
                argsExpanded  = map go args
            in case head_ of
                TyCon n ->
                    case Map.lookup n syns of
                        Just (TypeSynonym binders rhs)
                          | let arity = length binders
                          , length argsExpanded >= arity ->
                              let (forArity, extra) = splitAt arity argsExpanded
                                  subs = Map.fromList (zip binders forArity)
                                  expanded = applySubst subs rhs
                              in foldl TyApp expanded extra
                        _ -> foldl TyApp head_ argsExpanded
                _ -> foldl TyApp (go head_) argsExpanded
        TyCon n
            | Just (TypeSynonym [] rhs) <- Map.lookup n syns -> go rhs
            | otherwise -> t
        TyVar _      -> t
        TyArrow a b  -> TyArrow (go a) (go b)
        TyForall vs preds body -> TyForall vs preds (go body)

-- | One-hop synonym resolver.  Same as 'expandSyn' at the outer level
-- only — used by the trigger-finder to check if an annotation's head
-- is a synonym for something else.
resolveSynonymHop :: Map ByteString TypeSynonym -> Type -> Type
resolveSynonymHop = expandSyn

expandScheme :: Map ByteString TypeSynonym -> Scheme -> Scheme
expandScheme synonyms (Scheme vars preds body) =
    Scheme vars (map expandPred preds) (expandSyn synonyms body)
  where
    expandPred (Pred cls args) = Pred cls (map (expandSyn synonyms) args)
    expandPred (QPred vars' ctx body') =
        QPred vars' (map expandPred ctx) (expandPred body')

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
    trimSpaces = BC.dropWhile isAsciiSpace

    -- One simple type = list of atoms folded into a TyApp chain.
    parseT toks = do
        (a, rest) <- parseAtom toks
        (as, rest') <- parseAtoms rest
        let lhs = foldl TyApp a as
        case dropSpacesT rest' of
            (TArrow : afterArrow) -> do
                (rhs, final) <- parseT afterArrow
                pure (TyArrow lhs rhs, final)
            _ -> pure (lhs, rest')

    parseAtoms toks = case dropSpacesT toks of
        [] -> Just ([], toks)
        (TRParen : _)   -> Just ([], toks)
        (TRBracket : _) -> Just ([], toks)
        (TComma : _)    -> Just ([], toks)
        (TArrow : _)    -> Just ([], toks)
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
    | TArrow
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
            '-' | not (BC.null rest), BC.head rest == '>' ->
                    TArrow : tokenize (BC.tail rest)
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
