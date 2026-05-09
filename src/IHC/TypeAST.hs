-- | Type AST used by on-demand type inference (see
-- 'IHC.Elaborate').  Rank-1, single-parameter class constraints only —
-- enough to resolve ambiguous class dispatches in mtl-style code.
-- Not intended as a complete surface-Haskell type system.
module IHC.TypeAST
    ( Type(..)
    , Pred(..)
    , Scheme(..)
    , Subst
    , emptySubst
    , applySubst
    , applySubstPred
    , applySubstScheme
    , composeSubst
    , freeTyVars
    , freeTyVarsScheme
    , tyHead
    , tyArrowArgs
    , tyApps
    , isMonoType
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import IHC.AST (Name)

-- | Types as used by the elaborator.
data Type
    = TyVar   !Name             -- ^ type variable: @a@, @s@, @m@
    | TyCon   !Name             -- ^ type constructor: @Int@, @Maybe@, @StateT@
    | TyApp   !Type !Type       -- ^ application: @Maybe Int@ = @TyApp (TyCon "Maybe") (TyCon "Int")@
    | TyArrow !Type !Type       -- ^ function: @Int -> Bool@
    | TyForall ![Name] ![Pred] !Type
        -- ^ polymorphic: @forall a. Eq a => a -> Bool@
    deriving (Eq, Show)

-- | Class constraint: @Monad m@ = @Pred "Monad" (TyVar "m")@.
-- Multi-parameter constraints (e.g. @MonadState s m@) use 'TyApp' to
-- group the arguments: @Pred "MonadState" (TyApp (TyVar "s") (TyVar "m"))@
-- — callers flatten via 'predArgs' when matching instances.
data Pred
    = Pred !Name !Type
    -- | B.5b — quantified constraint, e.g. @forall a. Eq a => Eq (f a)@.
    -- @QPred vars body context@ binds 'vars' over a 'body' predicate
    -- that depends on 'context' predicates.  Today only the parser /
    -- scanner is aware of quantified constraints (B.5a tolerance);
    -- the solver that discharges 'QPred' by skolemising 'vars',
    -- attempting to discharge 'context' under the bound assumptions,
    -- and re-generalising lives in B.5b alongside the elaborator-
    -- integrated lowering.  No producer of 'QPred' exists yet.
    | QPred ![Name] ![Pred] !Pred
    deriving (Eq, Show)

-- | Quantified type.  Used for top-level bindings and class method
-- signatures.  E.g. @forall s a. Monad (StateT s m) => runStateT ::
-- StateT s m a -> s -> m (a, s)@.
data Scheme = Scheme ![Name] ![Pred] !Type
    deriving (Eq, Show)

-- | Substitution from type variable names to types.  Composed
-- left-to-right ('composeSubst' s1 s2 applies s1 first, then s2).
type Subst = Map Name Type

emptySubst :: Subst
emptySubst = Map.empty

-- | Apply a substitution to a type.  Cycle-safe via a "visited" set:
-- even a malformed cyclic substitution (@a → b@, @b → a@) terminates
-- by refusing to chase a TyVar that's already on the chain.
applySubst :: Subst -> Type -> Type
applySubst = applySubstVisited Set.empty

applySubstVisited :: Set Name -> Subst -> Type -> Type
applySubstVisited visited s t = case t of
    TyVar n
      | Set.member n visited -> TyVar n   -- break cycle
      | otherwise -> case Map.lookup n s of
          Just t' -> applySubstVisited (Set.insert n visited) s t'
          Nothing -> TyVar n
    TyCon n      -> TyCon n
    TyApp a b    -> TyApp (applySubstVisited visited s a) (applySubstVisited visited s b)
    TyArrow a b  -> TyArrow (applySubstVisited visited s a) (applySubstVisited visited s b)
    TyForall vs preds body ->
        -- Bound variables shadow the substitution.
        let s' = foldr Map.delete s vs
        in TyForall vs (map (applySubstPred s') preds) (applySubstVisited visited s' body)

applySubstPred :: Subst -> Pred -> Pred
applySubstPred s (Pred cls t) = Pred cls (applySubst s t)
applySubstPred s (QPred vs ctx body) =
    -- Bound type vars shadow the substitution under the QPred.
    let s' = foldr Map.delete s vs
    in QPred vs (map (applySubstPred s') ctx) (applySubstPred s' body)

applySubstScheme :: Subst -> Scheme -> Scheme
applySubstScheme s (Scheme vs preds body) =
    let s' = foldr Map.delete s vs
    in Scheme vs (map (applySubstPred s') preds) (applySubst s' body)

-- | Compose two substitutions: @composeSubst s1 s2@ is "apply s1, then s2".
-- Classical implementation: s2 ∘ s1 = map (applySubst s2) s1 ∪ s2.
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.union (Map.map (applySubst s2) s1) s2

-- | Free type variables appearing in a type.
freeTyVars :: Type -> Set Name
freeTyVars t = case t of
    TyVar n      -> Set.singleton n
    TyCon _      -> Set.empty
    TyApp a b    -> Set.union (freeTyVars a) (freeTyVars b)
    TyArrow a b  -> Set.union (freeTyVars a) (freeTyVars b)
    TyForall vs preds body ->
        Set.difference
            (Set.unions
                (freeTyVars body : map freeTyVarsPred preds))
            (Set.fromList vs)
  where
    freeTyVarsPred (Pred _ x) = freeTyVars x
    freeTyVarsPred (QPred vs ctx body) =
        Set.difference
            (Set.unions (freeTyVarsPred body : map freeTyVarsPred ctx))
            (Set.fromList vs)

freeTyVarsScheme :: Scheme -> Set Name
freeTyVarsScheme (Scheme vs preds body) =
    Set.difference
        (Set.unions (freeTyVars body : map predFreeTyVars preds))
        (Set.fromList vs)
  where
    predFreeTyVars (Pred _ x) = freeTyVars x
    predFreeTyVars (QPred qvs ctx p) =
        Set.difference
            (Set.unions (predFreeTyVars p : map predFreeTyVars ctx))
            (Set.fromList qvs)

-- | Leftmost head constructor of a type application chain.  Returns
-- 'Nothing' if the head is a type variable or an arrow.
--
-- > tyHead (Maybe Int)                 = Just "Maybe"
-- > tyHead (StateT s Identity)         = Just "StateT"
-- > tyHead (m a)                       = Nothing
-- > tyHead (a -> b)                    = Nothing
tyHead :: Type -> Maybe Name
tyHead = go
  where
    go (TyCon n)   = Just n
    go (TyApp a _) = go a
    go _           = Nothing

-- | Destructure @a -> b -> ... -> r@ into @([a, b, ...], r)@.
tyArrowArgs :: Type -> ([Type], Type)
tyArrowArgs (TyArrow a r) =
    let (xs, res) = tyArrowArgs r
    in (a : xs, res)
tyArrowArgs t = ([], t)

-- | Collect the tail of type-app arguments: @TyApp (TyApp M a) b@ → @(M, [a, b])@.
tyApps :: Type -> (Type, [Type])
tyApps = go []
  where
    go acc (TyApp a b) = go (b : acc) a
    go acc t           = (t, acc)

-- | True if the type contains no 'TyVar' nodes (monomorphic).
isMonoType :: Type -> Bool
isMonoType t = Set.null (freeTyVars t)
