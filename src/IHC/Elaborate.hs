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
    , schemeBelongsToClassMethod
    , elaborateOwned
    , elaborateOwnedMethod
    , elaborateOwnedWithScopedSigs
    , elaborateExpr
    , parseRawTypeExpr
    , resolveSynonymHop
    , foreignConstructorCollision
    , isStringLiteralExpr
    , isCharListType
    , fromStringResultTag
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
import IHC.Classes (ClassRegistry, currentDoCarrier, normalizeTyTag)
import IHC.ConstructorMetadata
    ( ConstructorTypeRegistry, constructorScheme
    , foreignConstructorCollision )
import IHC.StringUtils (isAsciiSpace)
import IHC.TypeAST
import IHC.TypeGlobals
    ( globalClassMethodNamesRef, globalAmbiguousSigsRef, globalMethodClassRef )
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

-- | A last-writer scheme is only safe for a class-method name when its
-- constraint is a class that actually declares the method.
-- @try@ is MonadParsec's method; @Exception e => IO a -> IO (Either e a)@
-- is a different top-level function.  Using that scheme as @try@'s type
-- pinned @Alternative (<|>)@ to IO, so @try p <|> q@ ran as IO and
-- @unParser@ saw a State# @(#,#)@ applied to @cok@.
schemeBelongsToClassMethod
    :: Set.Set ByteString
    -> Map ByteString [ByteString]
    -> Name
    -> Scheme
    -> Bool
schemeBelongsToClassMethod methodNames classMap name (Scheme _ preds _)
    | not (Set.member bare methodNames) = True
    | null preds = True
    | otherwise =
        let declared = map bareName (Map.findWithDefault [] bare classMap)
            predCls  = concatMap predClassNames preds
        in null declared || any (`elem` declared) predCls
  where
    bare = bareName name
    predClassNames (Pred cls _) = [bareName cls]
    predClassNames (QPred _ ctx p) = concatMap predClassNames (p : ctx)

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
    , ieMethodClasses    :: !(Map ByteString [ByteString])
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
    methodClasses    <- readIORef globalMethodClassRef
    ambiguousSigs    <- readIORef globalAmbiguousSigsRef
    let ienv = InferEnv
            { ieFresh    = fresh
            , ieSigs     = sigs
            , ieSynonyms = synonyms
            , ieClassReg = classReg
            , ieLocals   = Map.empty
            , ieClassMethodNames = classMethodNames
            , ieMethodClasses    = methodClasses
            , ieAmbiguousSigs    = ambiguousSigs
            , ieScopedSigs       = scopedSigs
            , ieConstructorTypes = constructorTypes
            , ieOwner            = owner
            }
    (e', t, _preds, sub, mWant) <- case expected of
        ExpectType want -> do
            -- Residual types from a previous InferFreely call reuse
            -- @$tN@ names.  A fresh source here also starts at @$t0@,
            -- so unifying @f [Char]@ with @Parsec $t1 $t0 $t2@ would
            -- occurs-check.  Freshen the expectation first.
            want' <- freshenType fresh (expandSyn synonyms want)
            (e0, t0, p0, s0) <- elaborateExpectedExpr ienv want' e
            pure (e0, t0, p0, s0, Just want')
        InferFreely -> do
            (e0, t0, p0, s0) <- elaborateExpr ienv e
            pure (e0, t0, p0, s0, Nothing)
    -- If we had an expected type, unify the result type.
    finalSub <- case mWant of
        Nothing -> pure sub
        Just wantResolved ->
            let tCur = applySubst sub t
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
    -- whose expected type is not a list / String is `fromString` at that
    -- result type.  The class method is result-polymorphic; the
    -- expected type (constructor field, annotation) chooses the
    -- instance.  No library or constructor name is special-cased.
    --
    -- Parser `[]` and desugared `""` share `EVar "[]"`.  Wrapping every
    -- non-[Char] nil in fromString left a leftover IsString function in
    -- `[Int]` / `[Flag]` constructor fields; `k \`elem\` xs` then died
    -- as an if-condition.  A list expected type is the nil constructor.
    (e, ty)
      | let ty' = expandSyn (ieSynonyms ienv) ty
      , not (isListType ty' || isCharListType ty')
      , isStringLiteralExpr e ->
            elaborateExpectedExpr ienv ty
                (EApp (EVar (BC.pack "fromString")) e)
    -- Unannotated integer literals at a non-Int/Integer expected type
    -- are `fromInteger` (GHC treats @0@ as @Num a => a@).  Derived Eq
    -- of a multi-ctor Num type (text fusion Size, local Hint) must not
    -- see a bare VInt.  No Size / Unknown / Text name list.
    --
    -- Skip when the expected head is the live do-carrier (ParsecT
    -- after `char`, IO, ST, …).  GHC only inserts fromInteger when
    -- `Num t` holds; there is no `Num (ParsecT e s m a)`, so a
    -- leftover `0` / `+ 4` inside T.pack must stay Int.  Wrapping it
    -- made int-spine Num.+ see (left=0 right=<ParsecT>).  Size / Hint
    -- are not the do-carrier, so they still wrap.
    (ELit (LInt n), ty)
      | not (isIntOrIntegerType ty) -> do
            skip <- expectedIsLiveCarrier ienv ty
            if skip
                then elaborateExpr ienv expr
                else elaborateExpectedExpr ienv ty
                    (EApp (EVar (BC.pack "fromInteger"))
                          (ELit (LInteger (fromIntegral n))))
    (ELit (LInteger _), ty)
      | not (isIntOrIntegerType ty) -> do
            skip <- expectedIsLiveCarrier ienv ty
            if skip
                then elaborateExpr ienv expr
                else elaborateExpectedExpr ienv ty
                    (EApp (EVar (BC.pack "fromInteger")) expr)
    -- `fromString x` (explicit, or the wrap above) at a concrete
    -- non-[Char] result type.  Pin the instance from the expected
    -- type so a monomorphic last-writer scheme (`String -> Text`)
    -- cannot leave the class-method dispatcher leftover.  Tag is the
    -- result constructor head — not a HostPreference / Pref name list.
    (EApp f x, ty)
      | not (isCharListType ty)
      , isFromStringExpr f
      , Just tag <- fromStringResultTag ty -> do
            (x', _xt, xPreds, xSub) <- elaborateExpr ienv x
            pure ( EApp (ETypedMethod (BC.pack "IsString")
                                      (BC.pack "fromString")
                                      tag)
                        x'
                 , ty
                 , xPreds
                 , xSub
                 )
    -- OverloadedStrings list literal at expected [T] (T not Char).
    -- `["h1"] :: [Text]` is `(:) "h1" []`, not a String.  The String
    -- case above skips any list expected type (empty `[]` in a
    -- `[Flag]` field is nil).  Push T onto each cons head so each
    -- `"h1"` becomes `fromString "h1"` at T.  No Text / Set name list.
    (e, ty)
      | Just elemTy <- listElementType (expandSyn (ieSynonyms ienv) ty)
      , not (isCharListType elemTy)
      , Just (hd, tl) <- splitListCons e -> do
            (hd', hdTy, hdPreds, hdSub) <-
                elaborateExpectedExpr ienv elemTy hd
            sHd <- either (throwIO . UnificationFailure) pure
                (unify hdSub (applySubst hdSub elemTy)
                             (applySubst hdSub hdTy))
            let ienvTl = applySubstIenv sHd ienv
                listTy = applySubst sHd ty
            (tl', tlTy, tlPreds, tlSub) <-
                elaborateExpectedExpr ienvTl listTy tl
            sTl <- either (throwIO . UnificationFailure) pure
                (unify tlSub (applySubst tlSub listTy)
                             (applySubst tlSub tlTy))
            let sFinal = composeSubst sHd sTl
            pure ( rebuildCons e hd' tl'
                 , applySubst sFinal ty
                 , map (applySubstPred sFinal) (hdPreds ++ tlPreds)
                 , sFinal
                 )
    -- `f ["h1", …] :: F T` — unary result `F T` (Set Text, not
    -- Map k v) and a list-of-string argument.  The list is `[T]`.
    -- IHP `parents = Set.fromList ["h1", …] :: Set Text` needs this:
    -- fromList's result annotation is Set, not [Text], so the cons
    -- case above never sees the elements.  `fail "tag"` is a String
    -- argument, not a list of strings; it does not match.
    -- Keep the callee as written.  Structural — no fromList / Set name list.
    (EApp f x, ty)
      | Just elemTy <- unaryTypeArg (expandSyn (ieSynonyms ienv) ty)
      , not (isCharListType elemTy)
      , isListOfStringLits x -> do
            let listTy = TyApp (TyCon (BC.pack "[]")) elemTy
            (x', xt, xPreds, xSub) <-
                elaborateExpectedExpr ienv listTy x
            sArg <- either (throwIO . UnificationFailure) pure
                (unify xSub (applySubst xSub listTy)
                            (applySubst xSub xt))
            pure ( EApp f x'
                 , applySubst sArg ty
                 , map (applySubstPred sArg) xPreds
                 , sArg
                 )
    -- Unary class-method application (`return ()`, `pure x`, `fail s`)
    -- at a concrete `M t` field or annotation.  defaultSettings stores
    -- `settingsBeforeMainLoop = return ()`; a last-writer monomorphic
    -- scheme leaves a leftover function and runIO silent-exits.  Same
    -- rule as the nullary class-method case below and as fromString at
    -- a constructor field: the expected monad head is the instance tag.
    -- Not a Settings / method name list.
    (EApp f x, ty)
      | Just method <- classMethodAppHead ienv f
      , Just (monadTy, argTy) <- splitMonadType (expandSyn (ieSynonyms ienv) ty)
      , Just headName <- tyHead (expandSyn (ieSynonyms ienv) monadTy)
      , let tag = bareName headName
      , not (BC.null tag) -> do
            clsMap <- readIORef globalMethodClassRef
            case Map.lookup method clsMap of
                Just (cls:_) -> do
                    (x', _xt, xPreds, xSub) <- elaborateExpectedExpr ienv argTy x
                    pure ( EApp (ETypedMethod cls method tag) x'
                         , ty
                         , xPreds
                         , xSub
                         )
                _ -> do
                    (e', t, preds, sub) <- elaborateExpr ienv expr
                    sub' <- either (throwIO . UnificationFailure) pure
                        (unify sub ty t)
                    pure (e', applySubst sub' ty,
                        map (applySubstPred sub') preds, sub')
    -- `const (return ())` at `IO () -> IO ()` (settingsInstallShutdownHandler).
    -- The expected type is an arrow, not `M t`, so the case above misses
    -- the inner `return ()`.  Push the monad carrier from either side of
    -- the arrow into the argument.  Structural: any unary class-method
    -- argument under an arrow whose components include `M t`.
    (EApp f x, ty)
      | Just _ <- classMethodAppHead ienv (appHeadExpr x)
      , Just ioTy <- monadTypeIn (expandSyn (ieSynonyms ienv) ty) -> do
            (x', _xt, xPreds, xSub) <- elaborateExpectedExpr ienv ioTy x
            (f', _ft, fPreds, fSub) <- elaborateExpr (applySubstIenv xSub ienv) f
            let sub = composeSubst fSub xSub
            pure (EApp f' x', applySubst sub ty,
                map (applySubstPred sub) (xPreds ++ fPreds), sub)
    (EVar name, _)
      | Just (Scheme vars preds _) <- Map.lookup name (ieLocals ienv)
      , not (null vars) || not (null preds)
      , Just expectedBytes <- renderExpectedType expected -> do
        (_, actual, actualPreds, sub) <- elaborateVar ienv name
        sub' <- either (throwIO . UnificationFailure) pure
            (unify sub expected actual)
        pure (ETyApp (EVar name) expectedBytes, applySubst sub' expected,
            map (applySubstPred sub') actualPreds, sub')
    -- Nullary class methods such as @location :: Quasi m => m Loc@ have
    -- no standalone top-level signature (they live in the class decl).
    -- When the do-block expected type is @Q t@, pin the method to that
    -- monad instead of leaving a bare EVar for value-directed ParsecT.
    -- Dispatch is by the constructor head after synonym expansion
    -- (`ParsecT e s m` / `Parser` → `ParsecT`), not the closed
    -- 'typeDispatchTag' (no instance key).  Same structural rule as
    -- 'applyMethodSubst' on an open transformer type.  No name list
    -- of Parser / ParsecT.  Do not treat ParsecT as Q/Exp.
    (EVar name, _)
      | Set.member (bareName name) (ieClassMethodNames ienv)
      , Just (monadTy, _) <- splitMonadType expected
      , Just headName <- tyHead (expandSyn (ieSynonyms ienv) monadTy)
      , let tag = bareName headName
      , not (BC.null tag) -> do
            clsMap <- readIORef globalMethodClassRef
            case Map.lookup (bareName name) clsMap of
                Just [cls] ->
                    pure (ETypedMethod cls (bareName name) tag,
                        expected, [], emptySubst)
                _ -> do
                    (e', t, preds, sub) <- elaborateExpr ienv (EVar name)
                    sub' <- either (throwIO . UnificationFailure) pure
                        (unify sub expected t)
                    pure (e', applySubst sub' expected,
                        map (applySubstPred sub') preds, sub')
    (ELam name body, TyArrow argTy resultTy) -> do
        let ie = ienv { ieLocals = Map.insert name (Scheme [] [] argTy)
                                    (ieLocals ienv) }
        (body', bodyTy, preds, sub) <- elaborateExpectedExpr ie resultTy body
        sub' <- either (throwIO . UnificationFailure) pure
            (unify sub (applySubst sub resultTy) (applySubst sub bodyTy))
        pure (ELam name body', TyArrow (applySubst sub' argTy)
            (applySubst sub' bodyTy), map (applySubstPred sub') preds, sub')
    (ELet bs body, _) -> elaborateLetExpected ienv bs expected body
    (EDo stmts, _) -> elaborateDo ienv (Just expected) stmts
    (ECase scrut alts, _) -> do
        (scrut', scrutTy, scrutPreds, scrutSub) <- elaborateExpr ienv scrut
        (alts', altPreds, finalSub) <- elaborateExpectedAlts
            (applySubstIenv scrutSub ienv) (applySubst scrutSub scrutTy)
            expected scrutSub alts
        pure (ECase scrut' alts', applySubst finalSub expected,
            map (applySubstPred finalSub) (scrutPreds ++ altPreds), finalSub)
    (EDo stmts, _) -> do
        (e', t, preds, sub) <- elaborateDo ienv (Just expected) stmts
        -- Keep the expected carrier on the do-block so evalDo does
        -- not default a function-shaped first action to ParsecT.
        case renderExpectedType expected of
            Just tyBytes ->
                pure (ETyApp e' tyBytes, expected, preds, sub)
            Nothing ->
                pure (e', t, preds, sub)
    -- [| e |] evaluates to a raw TH Exp tree.  When the expected type
    -- is Q t (runQ's argument, a `:: Q Exp` annotation), inhabit Q via
    -- `$qWrap (pure quote)` — the same injection Q-do uses so we never
    -- look up the name Q (Queue collision) and never pin `pure` to
    -- Applicative Q (`pure x = Q (pure x)` re-enters).  Unconstrained
    -- / Exp-expected quotes stay raw so `$(pure [| 42 |])` keeps
    -- working.  Not a runQ shim; not ParsecT-as-Q.
    (EQuote inner, ty)
      | expectedCarrierIsQ ienv ty ->
            pure ( EApp (EVar (BC.pack "$qWrap"))
                        (EApp (EVar (BC.pack "pure")) (EQuote inner))
                 , ty
                 , []
                 , emptySubst
                 )
    -- `listE` / `litE` / `appE` / `varE` already inhabit `m Exp` (the
    -- source body defaults to IO).  Wrap the application as Q without
    -- an extra `pure` — that would nest `Q (IO Exp)`.  Do not wrap
    -- `pure` / `return` / `$qWrap` heads: those already inhabit Q
    -- (`runQ (pure 42)`, `$qWrap (pure [| e |])`).  Not a combinator
    -- name list; quotes stay on the `$qWrap (pure quote)` path above.
    --
    -- Unify the residual result with expected Q so `listE :: [m Exp]
    -- -> m Exp` instantiates `m ~ Q` and the argument list is
    -- `[Q Exp]`.  Cons-list elaboration then wraps each raw quote
    -- (`sequenceA` of `[Exp]` is the leftover function).  If a quote
    -- (or `[Q t]` of quotes) already inhabits Q, do not wrap the
    -- application again — that nests `Q (Q a)` (`$(pure [| 42 |])`
    -- must stay 42).
    (EApp f x, ty)
      | expectedCarrierIsQ ienv ty ->
            elaborateQApp ienv ty (isQInhabitingHead f) f x
    _ -> elaborateExpr ienv expr

-- | Elaborate @f x@ at expected @Q t@.  Infer @f@, unify its result
-- with @Q t@, push the residual domain into @x@ (so a quote / list of
-- quotes sees @Q@), then wrap the application only when the arguments
-- did not already inhabit Q.
elaborateQApp
    :: InferEnv -> Type -> Bool -> Expr -> Expr
    -> IO (Expr, Type, [Pred], Subst)
elaborateQApp ienv ty alreadyQ f x = do
    inferred <- try (elaborateExpr ienv f)
        :: IO (Either SomeException (Expr, Type, [Pred], Subst))
    case inferred of
        Left _ -> fallback
        Right (f', ft, fPreds, s1) -> do
            let (doms, result) = tyArrowArgs (applySubst s1 ft)
                sU = case unify s1 (applySubst s1 result) (applySubst s1 ty) of
                    Right s -> s
                    Left _  -> s1
            case doms of
                (argTy:_) -> do
                    let argTy' = applySubst sU argTy
                        ienvX  = applySubstIenv sU ienv
                    elabX <- try (elaborateExpectedExpr ienvX argTy' x)
                        :: IO (Either SomeException (Expr, Type, [Pred], Subst))
                    case elabX of
                        Left _ -> fallback
                        Right (x', _xt, xPreds, s2) -> do
                            let sub   = composeSubst sU s2
                                preds = map (applySubstPred sub)
                                    (fPreds ++ xPreds)
                            -- `sequenceA` of `[Q Exp]` inside source
                            -- `listE` is leftover (`pure []` in
                            -- Traversable [] has no splice-root
                            -- expected Q).  Explicit Q-do of the same
                            -- quotes is GREEN.  A known `[Q Exp]` at
                            -- expected `Q Exp` is that do: bind each
                            -- action, `pure (ListE es)`.  Not a
                            -- listE / sequenceA name.  Empty `[]` is
                            -- the same do with no binds, but only
                            -- when the unified domain is `[Q Exp]`.
                            -- `runQ` only supplies `Q a`; after
                            -- unifying `listE :: [m Exp] -> m Exp`
                            -- the residual is `Q Exp`.  Check the
                            -- substituted type so already-Q elements
                            -- (`litE`) take this path.  `Q [Exp]`
                            -- (sequenceA) still misses.
                            case collectQActionList x' of
                                Just qs
                                  | not alreadyQ
                                  , expectedIsQExp ienv (applySubst sub ty)
                                  , not (null qs)
                                    || expectedIsListOfQExp ienv argTy' ->
                                    pure ( qDoListE qs, ty, preds, sub )
                                _ -> do
                                    let app = EApp f' x'
                                        wrapped
                                          | alreadyQ = app
                                          | exprHasQWrap x' = app
                                          | otherwise =
                                                EApp (EVar (BC.pack "$qWrap")) app
                                    pure (wrapped, ty, preds, sub)
                [] -> fallback
  where
    fallback
      | alreadyQ = elaborateExpr ienv (EApp f x)
      | otherwise =
            pure ( EApp (EVar (BC.pack "$qWrap")) (EApp f x)
                 , ty
                 , []
                 , emptySubst
                 )

-- | Heads that already produce a Q value.  `pure` / `return` are the
-- same result-poly injection Q-do uses; `$qWrap` is the synthetic
-- constructor wrapper.  Matching them here avoids Q (Q a).
isQInhabitingHead :: Expr -> Bool
isQInhabitingHead (EVar n)
    | n == BC.pack "$qWrap" = True
    | isUpperHead (bareName n) = True  -- Q constructor (`pure x = Q (pure x)`)
    | let b = bareName n
    = b == BC.pack "pure" || b == BC.pack "return"
isQInhabitingHead (ETyApp inner _) = isQInhabitingHead inner
isQInhabitingHead (ELocalSig _ inner) = isQInhabitingHead inner
isQInhabitingHead (ETypedMethod _ method _) =
    method == BC.pack "pure" || method == BC.pack "return"
isQInhabitingHead (EConstrainedValue inner _) = isQInhabitingHead inner
isQInhabitingHead (EApp f _) = isQInhabitingHead f
isQInhabitingHead _ = False

-- | True when a sub-tree already injected Q via `$qWrap`.  Used so
-- `listE [ $qWrap (pure [| e |]), … ]` is not wrapped again.
exprHasQWrap :: Expr -> Bool
exprHasQWrap (EApp (EVar n) _)
    | n == BC.pack "$qWrap" = True
exprHasQWrap (EApp f x) = exprHasQWrap f || exprHasQWrap x
exprHasQWrap (ETyApp e _) = exprHasQWrap e
exprHasQWrap (ELocalSig _ e) = exprHasQWrap e
exprHasQWrap (EConstrainedValue e _) = exprHasQWrap e
exprHasQWrap (ELam _ e) = exprHasQWrap e
exprHasQWrap (ELet bs e) =
    exprHasQWrap e || any (exprHasQWrap . snd) bs
exprHasQWrap (ETuple es) = any exprHasQWrap es
exprHasQWrap (EIf c t e) = exprHasQWrap c || exprHasQWrap t || exprHasQWrap e
exprHasQWrap (ECase s as) =
    exprHasQWrap s || any (\(Alt _ e) -> exprHasQWrap e) as
exprHasQWrap (EDo ss) = any stmtHasQWrap ss
exprHasQWrap _ = False

stmtHasQWrap :: Stmt -> Bool
stmtHasQWrap (SExpr e) = exprHasQWrap e
stmtHasQWrap (SBind _ e) = exprHasQWrap e
stmtHasQWrap (SBangBind _ e) = exprHasQWrap e
stmtHasQWrap (SLet bs) = any (exprHasQWrap . snd) bs
stmtHasQWrap (SImplicitLet bs) = any (exprHasQWrap . snd) bs

-- | Expected type is `Q Exp` (qualified or not).  The splice / runQ
-- result, not `Q [Exp]` (sequenceA's result) and not ParsecT.
expectedIsQExp :: InferEnv -> Type -> Bool
expectedIsQExp ienv ty =
    expectedCarrierIsQ ienv ty
    && case splitMonadType (expandSyn (ieSynonyms ienv) ty) of
        Just (_, arg) ->
            case tyHead (expandSyn (ieSynonyms ienv) arg) of
                Just n -> bareName n == BC.pack "Exp"
                Nothing -> False
        Nothing -> False

-- | Argument type is `[Q Exp]` (list of Q actions).  Empty `[]` at
-- this type is nil of that list, so `f []` at expected `Q Exp` is
-- the empty-list isolate of sequenceA — not a combinator name.
expectedIsListOfQExp :: InferEnv -> Type -> Bool
expectedIsListOfQExp ienv ty =
    case listElementType (expandSyn (ieSynonyms ienv) ty) of
        Just elemTy -> expectedIsQExp ienv elemTy
        Nothing -> False

-- | Variables that appear as splice holes (`$x` / `$(x)`), including
-- inside quotes.  Their rhs is a Q action — splice of that action
-- must run it.  Quotes themselves stay raw Exp.
splicedVarNames :: Expr -> [Name]
splicedVarNames = go
  where
    go (ESplice (EVar n)) = [n]
    go (ESplice e) = go e
    go (EQuote e) = go e
    go (EApp f x) = go f ++ go x
    go (ELam _ e) = go e
    go (ELet bs e) = concatMap (go . snd) bs ++ go e
    go (ECase s as) = go s ++ concat [go e | Alt _ e <- as]
    go (EIf c t e) = go c ++ go t ++ go e
    go (EDo ss) = concatMap goStmt ss
    go (ENeg e) = go e
    go (ETuple es) = concatMap go es
    go (ETyApp e _) = go e
    go (ELocalSig _ e) = go e
    go (EConstrainedValue e _) = go e
    go (EImplicitLet bs e) = concatMap (go . snd) bs ++ go e
    go (ERecordCon _ fs) = concatMap (go . snd) fs
    go (ERecordUpdate e fs) = go e ++ concatMap (go . snd) fs
    go _ = []
    goStmt (SExpr e) = go e
    goStmt (SBind _ e) = go e
    goStmt (SBangBind _ e) = go e
    goStmt (SLet bs) = concatMap (go . snd) bs
    goStmt (SImplicitLet bs) = concatMap (go . snd) bs

-- | Body of `let x = rhs in x` (plus leftover @e \@T@ wrappers).
letBodyVar :: Expr -> Maybe Name
letBodyVar (EVar n) = Just n
letBodyVar (ETyApp e _) = letBodyVar e
letBodyVar (ELocalSig _ e) = letBodyVar e
letBodyVar (EConstrainedValue e _) = letBodyVar e
letBodyVar _ = Nothing

-- | Cons spine whose every head already inhabits Q (`$qWrap` or
-- `pure` / `return`).  Nil is `Just []`.
collectQActionList :: Expr -> Maybe [Expr]
collectQActionList (ETyApp e _) = collectQActionList e
collectQActionList (ELocalSig _ e) = collectQActionList e
collectQActionList (EVar n)
    | bareName n == BC.pack "[]" = Just []
collectQActionList e
    | Just (h, t) <- splitListCons e
    , isQInhabitingHead h || exprHasQWrap h
    = (h :) <$> collectQActionList t
collectQActionList _ = Nothing

-- | Q-do of a known `[Q Exp]`: the GREEN isolate of `listE`'s
-- `sequenceA`.  `$qWrap (pure (ListE es))` matches Q-do's last
-- `pure`.  Empty has no binds — emit that last `pure` directly so
-- evalDo does not default a carrier-less `pure` to leftover.
qDoListE :: [Expr] -> Expr
qDoListE [] =
    EApp (EVar (BC.pack "$qWrap"))
         (EApp (EVar (BC.pack "pure"))
               (EApp (EVar (BC.pack "ListE")) (EVar (BC.pack "[]"))))
qDoListE qs =
    let ns = [ BC.pack ("$qel" <> show i) | i <- [1 :: Int .. length qs] ]
        binds = zipWith SBind ns qs
        list = foldr (\n acc -> EApp (EApp (EVar (BC.pack ":")) (EVar n)) acc)
                     (EVar (BC.pack "[]")) ns
        body = EApp (EVar (BC.pack "pure"))
                    (EApp (EVar (BC.pack "ListE")) list)
    in EDo (binds ++ [SExpr body])

isUpperHead :: Name -> Bool
isUpperHead n = case BC.uncons n of
    Just (c, _) -> c >= 'A' && c <= 'Z'
    Nothing -> False

-- | True when the expected type's monadic head is TH's @Q@ (qualified
-- or not).  Synonym expansion first, then the constructor head — not
-- 'typeDispatchTag', and not a ParsecT/Parser name list.
expectedCarrierIsQ :: InferEnv -> Type -> Bool
expectedCarrierIsQ ienv ty =
    case tyHead (expandSyn (ieSynonyms ienv) carrier) of
        Just n -> bareName n == BC.pack "Q"
        Nothing -> False
  where
    carrier = case splitMonadType (expandSyn (ieSynonyms ienv) ty) of
        Just (monadTy, _) -> monadTy
        Nothing -> ty

-- | [Char] after synonym expansion.  String is expanded by the
-- caller via 'expandSyn' before 'elaborateExpectedExpr'.
isCharListType :: Type -> Bool
isCharListType (TyApp (TyCon n) (TyCon e)) =
    (n == BC.pack "[]" || n == BC.pack "List") && e == BC.pack "Char"
isCharListType (TyCon n) = n == BC.pack "String"
isCharListType _ = False

-- | Any list type @[] a@ / @List a@ (qualified or not).  Empty @[]@ at
-- this expected type is the nil constructor, not @fromString ""@.
isListType :: Type -> Bool
isListType ty = case tyHead ty of
    Just n ->
        let b = bareName n
        in b == BC.pack "[]" || b == BC.pack "List"
    Nothing -> False

-- | Element type of @[] a@ / @List a@.  @Set a@ / @F a@ is not a list.
listElementType :: Type -> Maybe Type
listElementType ty = case tyApps ty of
    (TyCon n, [elemTy])
      | let b = bareName n
      , b == BC.pack "[]" || b == BC.pack "List" -> Just elemTy
    _ -> Nothing

-- | Single type argument of a unary constructor application
-- (@Set Text@, @Maybe a@).  @Map k v@ / @ParsecT e s m a@ have more
-- than one argument and are not this shape.
unaryTypeArg :: Type -> Maybe Type
unaryTypeArg ty = case tyApps ty of
    (TyCon _, [arg]) -> Just arg
    _ -> Nothing

-- | Desugared @(:) h t@, including a leftover @e \@T@ wrapper and a
-- qualified cons.  Not a String: @"h1"@ is a cons of 'LChar', which
-- still matches — callers that want a list of strings must also
-- require 'isListOfStringLits'.
splitListCons :: Expr -> Maybe (Expr, Expr)
splitListCons (ETyApp inner _) = splitListCons inner
splitListCons (EApp (EApp (EVar cons) h) t)
    | isConsName cons = Just (h, t)
splitListCons _ = Nothing

isConsName :: Name -> Bool
isConsName cons =
    let b = bareName cons
    in b == BC.pack ":" || BC.isSuffixOf (BC.pack ".:") cons

rebuildCons :: Expr -> Expr -> Expr -> Expr
rebuildCons orig h t = case stripConsTy orig of
    EApp (EApp cons _) _ -> EApp (EApp cons h) t
    _ -> EApp (EApp (EVar (BC.pack ":")) h) t
  where
    stripConsTy (ETyApp inner _) = stripConsTy inner
    stripConsTy e = e

-- | @["h1", "div"]@ — cons chain whose heads are string literals.
-- @"h1"@ itself is a String, not a list of strings: 'fail "tag"'
-- must not look like a list argument of fromStrings.
isListOfStringLits :: Expr -> Bool
isListOfStringLits e
    | isStringLiteralExpr e = False
    | Just (h, t) <- splitListCons e =
        isStringLiteralExpr h && isListOfStringLitsTail t
    | otherwise = False

isListOfStringLitsTail :: Expr -> Bool
isListOfStringLitsTail (ETyApp inner _) = isListOfStringLitsTail inner
isListOfStringLitsTail e
    | isNilExpr e = True
    | isListOfStringLits e = True
    | otherwise = False

isNilExpr :: Expr -> Bool
isNilExpr (ETyApp inner _) = isNilExpr inner
isNilExpr (EVar n) = bareName n == BC.pack "[]"
isNilExpr _ = False

-- | Boxed Int / Integer — the default type of an unannotated integer
-- literal.  Any other expected head (Hint, Size, Word8, …) needs
-- fromInteger.
isIntOrIntegerType :: Type -> Bool
isIntOrIntegerType ty = case tyHead ty of
    Just n ->
        let b = bareName n
        in b == BC.pack "Int" || b == BC.pack "Integer"
            || b == BC.pack "I#" || b == BC.pack "Int#"
    Nothing -> False

-- | True when the expected type's constructor head is the do-carrier
-- currently published by eval (`ParsecT` after a parser bind, …).
-- Structural: uses the live carrier tag, not a ParsecT/Parser name list.
expectedIsLiveCarrier :: InferEnv -> Type -> IO Bool
expectedIsLiveCarrier ienv ty = do
    mDo <- currentDoCarrier
    let expanded = expandSyn (ieSynonyms ienv) ty
        headTag = fmap (normalizeTyTag . bareName) (tyHead expanded)
    pure $ case (mDo, headTag) of
        (Just c, Just t) ->
            not (BC.null c) && normalizeTyTag c == t
        _ -> False

-- | Parser-desugared @"…"@ is a cons-chain of 'ELit' 'LChar', or the
-- original 'LStr' if desugar has not run.
isStringLiteralExpr :: Expr -> Bool
isStringLiteralExpr (ETyApp inner _) = isStringLiteralExpr inner
isStringLiteralExpr (ELit (LStr _)) = True
isStringLiteralExpr (ELit (LChar _)) = False
isStringLiteralExpr (EVar n) = n == BC.pack "[]"
isStringLiteralExpr (EApp (EApp (EVar cons) (ELit (LChar _))) rest)
    | cons == BC.pack ":" || cons == BC.pack "GHC.Types.:" =
        isStringLiteralExpr rest
isStringLiteralExpr (EApp (EApp (EVar cons) (ELit (LChar _))) rest)
    | BC.isSuffixOf (BC.pack ".:") cons = isStringLiteralExpr rest
isStringLiteralExpr _ = False

-- | Head of a `fromString` application, including an already-pinned
-- 'ETypedMethod' and a leftover @e \@T@ wrapper.
isFromStringExpr :: Expr -> Bool
isFromStringExpr (EVar n) = bareName n == BC.pack "fromString"
isFromStringExpr (ETyApp inner _) = isFromStringExpr inner
isFromStringExpr (ETypedMethod _ method _) = method == BC.pack "fromString"
isFromStringExpr _ = False

-- | Unary class-method head (`return`, `pure`, `fail`, …), including
-- an already-pinned 'ETypedMethod' and a leftover @e \@T@ wrapper.
classMethodAppHead :: InferEnv -> Expr -> Maybe Name
classMethodAppHead ienv (EVar n)
    | Set.member (bareName n) (ieClassMethodNames ienv) = Just (bareName n)
    | otherwise = Nothing
classMethodAppHead ienv (ETyApp inner _) = classMethodAppHead ienv inner
classMethodAppHead _ (ETypedMethod _ method _) = Just method
classMethodAppHead _ _ = Nothing

appHeadExpr :: Expr -> Expr
appHeadExpr (EApp f _) = appHeadExpr f
appHeadExpr (ETyApp inner _) = appHeadExpr inner
appHeadExpr e = e

-- | A concrete `M t` anywhere in an expected type, including under
-- arrows (`IO () -> IO ()`).  First-wins so the domain of a setter
-- field is preferred when both sides match.
monadTypeIn :: Type -> Maybe Type
monadTypeIn ty = case splitMonadType ty of
    Just (monadTy, argTy) -> Just (TyApp monadTy argTy)
    Nothing -> case ty of
        TyArrow a b -> monadTypeIn a `orElse` monadTypeIn b
        TyApp f x -> monadTypeIn f `orElse` monadTypeIn x
        _ -> Nothing
  where
    orElse (Just a) _ = Just a
    orElse Nothing b = b

-- | Instance tag for result-polymorphic 'fromString'.
-- Nullary heads (`Pref`, `HostPreference`, `ByteString`) use the
-- constructor name.  Applications (`CI ByteString`) keep the
-- structural tag so they do not steal `instance IsString (CI a)`
-- registered under the `CI` constructor — that dictionary needs an
-- inner IsString and is leftover if selected here.
fromStringResultTag :: Type -> Maybe Name
fromStringResultTag ty = case tyApps ty of
    (TyCon n, []) ->
        let bare = bareName n
        in if BC.null bare then Nothing else Just bare
    (TyCon _, _ : _) ->
        let tag = typeDispatchTag ty
        in if BC.null tag then Nothing else Just tag
    _ -> do
        n <- tyHead ty
        let bare = bareName n
        if BC.null bare then Nothing else Just bare

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
            ft' = applySubst s1 ft
        -- Bidirectional application inference: when the callee already
        -- exposes its argument type, use that as context for the argument.
        -- In particular, @48 + nibble@ must keep the first literal
        -- overloaded until @nibble :: Word8@ fixes the shared @Num a@;
        -- defaulting the literal to Int here makes the otherwise valid
        -- expression fail elaboration and later misdispatch Storable.poke.
        (x', xt, xPreds, s2) <- case ft' of
            TyArrow expectedArg _ ->
                elaborateExpectedExpr ienvX expectedArg x
            _ -> elaborateExpr ienvX x
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

    EDo stmts -> elaborateDo ienv Nothing stmts

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
                        Nothing
                            -- A foreign same-named constructor (Queue's Q)
                            -- must not supply the scheme for this owner's
                            -- use of the name (TH's Q).  Infer freely.
                            -- Do not treat ParsecT as Q/Exp.
                            | foreignConstructorCollision
                                (ieConstructorTypes ienv) (ieOwner ienv) name
                            -> pure Nothing
                            | otherwise -> lookupSig
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
            | not (schemeMatchesMethod s) -> Nothing
            | isQualifiedName name
           || not (isAmbiguousSig ienv (bareName name)) -> Just s
            | otherwise -> Nothing
        Nothing
            -- Qualified ordinary bindings share a class-method name
            -- with last-writer (`Set.fromList` vs `IsList.fromList`).
            -- InferFreely of that class scheme at @Set Text@ walks
            -- @Item@ and hangs before main.  Exact scheme only —
            -- the facade extra hop is the owner's scheme, not this
            -- last-writer.  No fromList name list.
            | isQualifiedName name -> Nothing
            | isAmbiguousSig ienv (bareName name) -> Nothing
            | otherwise -> case Map.lookup (bareName name) (ieSigs ienv) of
                Just s | schemeMatchesMethod s -> Just s
                _ -> Nothing
    schemeMatchesMethod =
        schemeBelongsToClassMethod (ieClassMethodNames ienv)
            (ieMethodClasses ienv) (bareName name)

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
      , isActualClassMethod ienv methodName
      , schemeBelongsToClassMethod (ieClassMethodNames ienv)
            (ieMethodClasses ienv) methodName
            (Scheme [] preds body) ->
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
        spliceNs = splicedVarNames body
        bodyVar  = letBodyVar body
    (bs', bindPreds, bindSub) <-
        inferBinds seededEnv emptySubst [] [] bs preseed spliceNs bodyVar
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
    -- Splice of a let-bound Q action (`let x = listE [] in [| $x |]`)
    -- must elaborate the rhs at Q Exp.  A body that is just the
    -- binding (`let x = listE [] in x`) inherits the let expected
    -- type.  Infer freely otherwise — do not wrap every quote as Q.
    qExpTy = TyApp (TyCon (BC.pack "Q")) (TyCon (BC.pack "Exp"))
    inferBinds _ sub accE accP [] _ _ _ = pure (reverse accE, accP, sub)
    inferBinds ie sub accE accP ((n, rhs) : rest) ((_, seeded) : preRest)
            spliceNs bodyVar = do
        let rhsWant
                | n `elem` spliceNs = Just qExpTy
                | bodyVar == Just n = Just expected
                | otherwise         = Nothing
            inferRhs = case rhsWant of
                Just want ->
                    elaborateExpectedExpr (applySubstIenv sub ie)
                        (applySubst sub want) rhs
                Nothing ->
                    elaborateExpr (applySubstIenv sub ie) rhs
        inferred <- try inferRhs
            :: IO (Either SomeException (Expr, Type, [Pred], Subst))
        case inferred of
            Left _ -> inferBinds ie sub ((n, rhs) : accE) accP rest preRest
                spliceNs bodyVar
            Right (rhs', rhsT, preds, rhsSub) ->
                case unify rhsSub rhsT (applySubst rhsSub seeded) of
                    Left _ -> inferBinds ie sub ((n, rhs) : accE) accP rest
                        preRest spliceNs bodyVar
                    Right unified ->
                        inferBinds ie (composeSubst sub unified)
                            ((n, rhs') : accE) (preds ++ accP) rest preRest
                            spliceNs bodyVar
    inferBinds _ _ _ _ _ _ _ _ = pure ([], [], emptySubst)

unsnocStmts :: [Stmt] -> Maybe ([Stmt], Stmt)
unsnocStmts [] = Nothing
unsnocStmts [s] = Just ([], s)
unsnocStmts (s:ss) = do
    (prefix, lastS) <- unsnocStmts ss
    Just (s:prefix, lastS)

-- | Split @m a@ / @ParsecT e s a@ into the monad constructor application
-- and its result argument.
splitMonadType :: Type -> Maybe (Type, Type)
splitMonadType ty = case tyApps ty of
    (h, args@(_:_)) -> Just (foldl TyApp h (init args), last args)
    _ -> Nothing

-- | Do-block: when an enclosing expected type is available (@Q Exp@
-- from a QuasiQuoter field, @ParsecT e s a@ from a parser signature)
-- push the monad into each statement so @location@ / @pure@ unify
-- with that carrier instead of staying a free @m@ that later
-- defaults to ParsecT at eval.  Without an expected type the outer
-- type stays a fresh tyvar (historical InferFreely path).
elaborateDo :: InferEnv -> Maybe Type -> [Stmt] -> IO (Expr, Type, [Pred], Subst)
elaborateDo ienv mExpected stmts = case (mExpected, unsnocStmts stmts) of
    (Just expected, Just (prefix, lastStmt)) -> do
        let mMonad = fst <$> splitMonadType expected
        (prefix', ie', preds, sub) <-
            goStmtsPrefix ienv emptySubst [] [] prefix mMonad
        (last', _, lastPreds, lastSub) <-
            goStmt ie' sub lastStmt (Just (applySubst sub expected))
        let sub' = lastSub
        pure ( EDo (prefix' ++ [last'])
             , applySubst sub' expected
             , map (applySubstPred sub') (preds ++ lastPreds)
             , sub'
             )
    _ -> do
        (stmts', _, preds, sub) <- goStmtsPrefix ienv emptySubst [] [] stmts Nothing
        t <- case mExpected of
            Just expected -> pure (applySubst sub expected)
            Nothing -> TyVar <$> freshVar (ieFresh ienv)
        pure (EDo stmts', t, preds, sub)
  where
    goStmtsPrefix ie sub accS accP [] _ =
        pure (reverse accS, ie, reverse accP, sub)
    goStmtsPrefix ie sub accS accP (s : rest) mMonad = do
        stmtExpected <- case mMonad of
            Nothing -> pure Nothing
            Just monadTy -> do
                a <- TyVar <$> freshVar (ieFresh ie)
                pure (Just (TyApp (applySubst sub monadTy) a))
        (s', ie', preds, sub') <- goStmt ie sub s stmtExpected
        goStmtsPrefix ie' sub' (s' : accS) (preds ++ accP) rest mMonad

    goStmt :: InferEnv -> Subst -> Stmt -> Maybe Type
           -> IO (Stmt, InferEnv, [Pred], Subst)
    goStmt ie sub stmt mStmtExpected = case stmt of
        SExpr e -> do
            (e', _t, preds, s') <- inferRhs ie sub e mStmtExpected
            pure (SExpr e', ie, preds, s')
        SBind name e -> do
            (e', eTy, preds, s') <- inferRhs ie sub e mStmtExpected
            let boundTy = case splitMonadType eTy of
                    Just (_, a) -> a
                    Nothing -> eTy
            let ie' = ie { ieLocals = Map.insert name
                                          (Scheme [] [] boundTy)
                                          (ieLocals ie) }
            pure (SBind name e', ie', preds, s')
        SBangBind name e -> do
            (e', eTy, preds, s') <- inferRhs ie sub e mStmtExpected
            let boundTy = case splitMonadType eTy of
                    Just (_, a) -> a
                    Nothing -> eTy
            let ie' = ie { ieLocals = Map.insert name
                                          (Scheme [] [] boundTy)
                                          (ieLocals ie) }
            pure (SBangBind name e', ie', preds, s')
        SLet bs -> do
            (bs', ie', preds, s') <- goLet (applySubstIenv sub ie) bs
            pure (SLet bs', ie', preds, composeSubst sub s')
        SImplicitLet bs -> do
            (bs', ie', preds, s') <- goLet (applySubstIenv sub ie) bs
            pure (SImplicitLet bs', ie', preds, composeSubst sub s')

    inferRhs ie sub e mStmtExpected = do
        (e', t, preds, s') <- case mStmtExpected of
            Just expected ->
                elaborateExpectedExpr (applySubstIenv sub ie)
                    (applySubst sub expected) e
            Nothing ->
                elaborateExpr (applySubstIenv sub ie) e
        pure (e', applySubst s' t, preds, composeSubst sub s')

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
    --
    -- Result-poly @pure@/@return@/@empty@ dispatch on the type constructor
    -- after synonym expansion (@Parser Char@ → @Parsec Void Text Char@ →
    -- @ParsecT e s Identity Char@ → @ParsecT@).  The closed
    -- 'typeDispatchTag' (@ParsecT Void Text Identity@) has no instance
    -- key.  Type-kind classes (IsString, Num) keep the structural tag.
    -- No name list of @Parser@/@ParsecT@.
    resolveTag :: Maybe Name -> Name -> Maybe Name
    resolveTag mMethod tag = case Map.lookup tag sub of
        Just ty ->
            let resolved = expandTypeSynonyms synonyms (applySubst sub ty)
            in if isResultPolyCarrier mMethod
                   then fmap bareName (tyHead resolved)
                   else if Set.null (freeTyVars resolved)
                       then Just (typeDispatchTag resolved)
                       -- Monad transformers keep free args (@ParsecT e s Identity@)
                       -- after pinning the constructor.  Dispatch only needs the
                       -- head; requiring a closed type reverted @pure@/@*>@ to
                       -- bare EVar and lost the ParsecT instance.
                       -- Same for TH @Q a@: do not treat ParsecT as Q/Exp.
                       else tyHead resolved
        Nothing
          | isHeadName tag ->
              let expanded = expandTypeSynonyms synonyms (TyCon tag)
              in if isResultPolyCarrier mMethod
                     then fmap bareName (tyHead expanded)
                     else Just (typeDispatchTag expanded)
          | otherwise      -> Nothing            -- unresolved placeholder type variable

    isResultPolyCarrier (Just n) =
        let b = bareName n
        in b == BC.pack "pure" || b == BC.pack "return" || b == BC.pack "empty"
    isResultPolyCarrier Nothing = False

    -- A resolved type head is a constructor: an uppercase name, or the list /
    -- tuple constructors.  Type-variable placeholders (lowercase, or the
    -- fresh-var @$t…@ names) are not heads.
    isHeadName t = case BC.uncons t of
        Just (c, _) -> (c >= 'A' && c <= 'Z') || c == '[' || c == '('
        Nothing     -> False

    go e = case e of
        ETypedMethod cls method tag ->
            case resolveTag (Just method) tag of
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

-- | Replace every free type variable with a fresh name from 'fs'.
-- Used so an expected type produced by a previous inference pass
-- cannot collide with this pass's @$tN@ stream.
freshenType :: FreshSource -> Type -> IO Type
freshenType fs ty = do
    let vs = Set.toList (freeTyVars ty)
    fresh <- mapM (\_ -> freshVar fs) vs
    pure (applySubst (Map.fromList (zip vs (map TyVar fresh))) ty)

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
                    case lookupTypeSynonym syns n of
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
            | Just (TypeSynonym [] rhs) <- lookupTypeSynonym syns n -> go rhs
            | otherwise -> t
        TyVar _      -> t
        TyArrow a b  -> TyArrow (go a) (go b)
        TyForall vs preds body -> TyForall vs preds (go body)

-- | One-hop synonym resolver.  Same as 'expandSyn' at the outer level
-- only — used by the trigger-finder to check if an annotation's head
-- is a synonym for something else.
resolveSynonymHop :: Map ByteString TypeSynonym -> Type -> Type
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
