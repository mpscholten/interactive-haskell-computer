-- | Type AST used by on-demand type inference (see
-- 'IHC.Elaborate').  Rank-1, single-parameter class constraints only —
-- enough to resolve ambiguous class dispatches in mtl-style code.
-- Not intended as a complete surface-Haskell type system.
module IHC.TypeAST
    ( Type(..)
    , Pred(..)
    , Scheme(..)
    , TypeSynonym(..)
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
    , typeDispatchTag
    , expandTypeSynonyms
    , expandScheme
    , lookupTypeSynonym
    , predsFromConstraintType
    , implicitParamPreds
    , isMonoType
    , schemeIsResultPolymorphic
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BC

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

-- | Type-synonym metadata with the declared LHS binder order preserved.
-- Substitution must follow this order; deriving it from the RHS free-variable
-- set is unsound for declarations such as @type Flip b a = Either a b@.
data TypeSynonym = TypeSynonym ![Name] !Type
    deriving (Eq, Show)

-- | A stable, structural registry key for a class parameter.  In
-- particular, applications retain their arguments: @[Char]@ and @[Markup]@
-- must not both collapse to the runtime list-constructor tag @[]@.
typeDispatchTag :: Type -> Name
typeDispatchTag = go
  where
    go (TyVar n) = n
    go (TyCon n) = n
    go t@(TyApp _ _) =
        let (h, args) = tyApps t
        in case (h, args) of
            (TyCon n, [a]) | n == BC.pack "[]" ->
                BC.concat [BC.pack "[", go a, BC.pack "]"]
            (TyCon n, as) | isTuple n ->
                BC.concat [BC.pack "(", BC.intercalate (BC.pack ",") (map go as), BC.pack ")"]
            _ -> BC.intercalate (BC.pack " ") (go h : map atom args)
    go (TyArrow a b) = BC.concat [atom a, BC.pack "->", go b]
    go (TyForall _ _ body) = go body

    -- Keep applications unambiguous when nested in another application.
    atom t@TyApp{} = BC.concat [BC.pack "(", go t, BC.pack ")"]
    atom t@TyArrow{} = BC.concat [BC.pack "(", go t, BC.pack ")"]
    atom t = go t

    isTuple n = BC.length n >= 2 && BC.head n == '(' && BC.last n == ')'
             && BC.all (== ',') (BC.init (BC.tail n))

-- | Recursively expand saturated type synonyms for registry keys, substituting
-- arguments in the declared LHS binder order retained by the scanner.
expandTypeSynonyms :: Map Name TypeSynonym -> Type -> Type
expandTypeSynonyms synonyms = expand Set.empty
  where
    expand seen t = case t of
        TyApp _ _ ->
            let (h, args0) = tyApps t
                args = map (expand seen) args0
            in case h of
                TyCon n
                    | Set.notMember n seen
                    , Just (TypeSynonym binders rhs) <- lookupTypeSynonym synonyms n
                    , let arity = length binders
                    , length args >= arity ->
                        let (used, extra) = splitAt arity args
                            rhs' = applySubst (Map.fromList (zip binders used)) rhs
                        in foldl TyApp (expand (Set.insert n seen) rhs') extra
                _ -> foldl TyApp (expand seen h) args
        TyCon n
            | Set.notMember n seen
            , Just (TypeSynonym [] rhs) <- lookupTypeSynonym synonyms n ->
                expand (Set.insert n seen) rhs
            | otherwise -> TyCon n
        TyVar n -> TyVar n
        TyArrow a b -> TyArrow (expand seen a) (expand seen b)
        TyForall vs ps body -> TyForall vs ps (expand seen body)

lookupTypeSynonym :: Map Name TypeSynonym -> Name -> Maybe TypeSynonym
lookupTypeSynonym syns n =
    case Map.lookup n syns of
        Just s -> Just s
        Nothing -> case BC.elemIndexEnd (toEnum (fromEnum '.')) n of
            Just i -> Map.lookup (BC.drop (i + 1) n) syns
            Nothing -> Nothing

-- | Predicates encoded in a constraint type: a leftover context
-- @forall. (?x :: T) => body@, an IP-only synonym RHS
-- @TyApp (TyCon "?name") T@, or a saturated class application
-- @Show a@.  Dummy bodies used to encode IP-only synonyms
-- (@Constraint@ / @()@) are not themselves class heads.
predsFromConstraintType :: Type -> [Pred]
predsFromConstraintType (TyForall _ ps _) | not (null ps) = ps
predsFromConstraintType (TyApp (TyCon n) ty)
    | not (BC.null n), BC.head n == '?' = [Pred n [ty]]
predsFromConstraintType t =
    case tyApps t of
        (TyCon n, args)
            | n /= BC.pack "Constraint" && n /= BC.pack "()" ->
                [Pred n args]
        _ -> []

-- | Expand class / constraint-synonym heads in a scheme's context.
-- @HasCallStack@ (nullary synonym for @(?callStack :: CallStack)@)
-- becomes the implicit-param predicate so callers can publish it.
expandScheme :: Map Name TypeSynonym -> Scheme -> Scheme
expandScheme synonyms (Scheme vars preds body) =
    Scheme vars (expandConstraintPreds synonyms preds)
                (expandTypeSynonyms synonyms body)

expandConstraintPreds :: Map Name TypeSynonym -> [Pred] -> [Pred]
expandConstraintPreds synonyms = concatMap (go Set.empty)
  where
    go seen (Pred cls args) =
        let args' = map (expandTypeSynonyms synonyms) args
        in case lookupTypeSynonym synonyms cls of
            Just (TypeSynonym binders rhs)
              | Set.notMember cls seen
              , length args' == length binders ->
                let rhs' = applySubst (Map.fromList (zip binders args')) rhs
                    expanded = expandTypeSynonyms synonyms rhs'
                    seen' = Set.insert cls seen
                in case predsFromConstraintType expanded of
                    [] -> [Pred cls args']
                    ps -> concatMap (go seen') ps
            _ -> [Pred cls args']
    go _ q@QPred{} = [q]

-- | Implicit-param predicates (@?name :: T@) in a context.  The
-- returned name is the IP map key (no leading @?@).
implicitParamPreds :: [Pred] -> [(Name, Type)]
implicitParamPreds = concatMap one
  where
    one (Pred n [ty])
      | not (BC.null n), BC.head n == '?' = [(BC.drop 1 n, ty)]
    one _ = []

-- | Class constraint: @Monad m@ = @Pred "Monad" [TyVar "m"]@.
-- Multi-parameter constraints retain their argument boundaries, e.g.
-- @MonadState s m@ = @Pred "MonadState" [TyVar "s", TyVar "m"]@.
-- This is distinct from a single applied argument such as
-- @C (f a)@ = @Pred "C" [TyApp (TyVar "f") (TyVar "a")]@.
data Pred
    = Pred !Name ![Type]
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
applySubstPred s (Pred cls ts) = Pred cls (map (applySubst s) ts)
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
    freeTyVarsPred (Pred _ xs) = Set.unions (map freeTyVars xs)
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
    predFreeTyVars (Pred _ xs) = Set.unions (map freeTyVars xs)
    predFreeTyVars (QPred qvs ctx p) =
        Set.difference
            (Set.unions (predFreeTyVars p : map predFreeTyVars ctx))
            (Set.fromList qvs)

-- | Leftmost head constructor of a type application chain.  Returns
-- 'Nothing' if the head is a type variable or an arrow.  Walks
-- 'TyForall' so a leftover context (`forall. (?x :: T) => Parsec …`)
-- still exposes the constructor head used as a do-carrier.
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
    go (TyForall _ _ b) = go b
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

-- | True when a constrained type variable appears in the *result*
-- of a scheme (@fromInteger :: Num a => Integer -> a@, @fromIntegral
-- :: (Integral a, Num b) => a -> b@).  @sizeOf :: Storable a => a -> Int@
-- is the opposite: @a@ lives only in the argument.
schemeIsResultPolymorphic :: Scheme -> Bool
schemeIsResultPolymorphic (Scheme _ preds body) =
    let (_, result) = tyArrowArgs body
        resultVars  = freeTyVars result
        predVars    = Set.unions (map predTyVars preds)
    in not (Set.null (Set.intersection resultVars predVars))
  where
    predTyVars (Pred _ ts) = Set.unions (map freeTyVars ts)
    predTyVars (QPred vs ctx p) =
        Set.difference
            (Set.unions (predTyVars p : map predTyVars ctx))
            (Set.fromList vs)
