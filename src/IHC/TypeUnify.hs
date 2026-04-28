-- | Robinson unification for 'IHC.TypeAST.Type'.  Used by the
-- on-demand elaborator in 'IHC.Elaborate'.
module IHC.TypeUnify
    ( UnifyError(..)
    , unify
    , mgu
    , bindVar
    , freshVar
    , FreshSource
    , newFreshSource
    , instantiate
    , generalize
    ) where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Set (Set)

import IHC.AST (Name)
import IHC.TypeAST

-- | Unification failure.  Callers in 'IHC.Elaborate' promote these
-- to an 'InferenceError' that surfaces as an 'IhcException'.
data UnifyError
    = OccursCheck !Name !Type
    | TypeMismatch !Type !Type
    deriving (Show)

instance Exception UnifyError

-- | Most-general unifier for two types.
mgu :: Type -> Type -> Either UnifyError Subst
mgu t1 t2 = case (t1, t2) of
    (TyVar n, _)      -> bindVar n t2
    (_, TyVar n)      -> bindVar n t1
    (TyCon a, TyCon b) | a == b -> Right emptySubst
    (TyApp f1 a1, TyApp f2 a2) -> do
        s1 <- mgu f1 f2
        s2 <- mgu (applySubst s1 a1) (applySubst s1 a2)
        Right (composeSubst s1 s2)
    (TyArrow a1 b1, TyArrow a2 b2) -> do
        s1 <- mgu a1 a2
        s2 <- mgu (applySubst s1 b1) (applySubst s1 b2)
        Right (composeSubst s1 s2)
    -- Forall-types only unify if alpha-equivalent.  In practice the
    -- elaborator instantiates foralls before unification so this case
    -- is rare.
    (TyForall v1 _ b1, TyForall v2 _ b2)
      | length v1 == length v2 -> do
          let rename = Map.fromList (zip v1 (map TyVar v2))
              b1'   = applySubst rename b1
          mgu b1' b2
    _ -> Left (TypeMismatch t1 t2)

-- | Bind a type variable to a type, after the occurs check.
bindVar :: Name -> Type -> Either UnifyError Subst
bindVar n t
    | TyVar m <- t, m == n = Right emptySubst
    | Set.member n (freeTyVars t) = Left (OccursCheck n t)
    | otherwise = Right (Map.singleton n t)

-- | Unify two types under an existing substitution.  The result is the
-- composed substitution.
unify :: Subst -> Type -> Type -> Either UnifyError Subst
unify s t1 t2 = do
    let t1' = applySubst s t1
        t2' = applySubst s t2
    s' <- mgu t1' t2'
    Right (composeSubst s s')

-- | Source of fresh type variable names for instantiation.
newtype FreshSource = FreshSource (IORef Int)

newFreshSource :: IO FreshSource
newFreshSource = FreshSource <$> newIORef 0

freshVar :: FreshSource -> IO Name
freshVar (FreshSource ref) = do
    n <- readIORef ref
    writeIORef ref (n + 1)
    pure (BC.pack ("$t" ++ show n))

-- | Instantiate a scheme by replacing all quantified vars with fresh
-- type variables.  Returns the fresh predicates and the body type.
instantiate :: FreshSource -> Scheme -> IO ([Pred], Type)
instantiate fs (Scheme vs preds body) = do
    fresh <- mapM (\_ -> freshVar fs) vs
    let sub = Map.fromList (zip vs (map TyVar fresh))
    pure ( map (applySubstPred sub) preds
         , applySubst sub body
         )

-- | Generalize a type by quantifying over any free type variables not
-- present in the surrounding context.  Used when binding variables
-- via @let@.
generalize :: Set Name -> [Pred] -> Type -> Scheme
generalize ctxVars preds body =
    let usedVars = Set.union (freeTyVars body)
                             (Set.unions (map predFreeVars preds))
        quantified = Set.toList (Set.difference usedVars ctxVars)
    in Scheme quantified preds body
  where
    predFreeVars (Pred _ t) = freeTyVars t
    -- B.5b: a quantified-constraint binder shadows its bound vars
    -- inside its own body / context.  No producer of QPred exists
    -- yet; this arm exists to keep -Wincomplete-patterns clean.
    predFreeVars (QPred qvs ctx p) =
        Set.difference
            (Set.unions (predFreeVars p : map predFreeVars ctx))
            (Set.fromList qvs)
