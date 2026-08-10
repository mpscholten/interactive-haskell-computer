-- | Constructor field-type metadata and expected-type instantiation.
module IHC.ConstructorMetadata
    ( ConstructorIdentity(..)
    , ConstructorTypeMetadata(..)
    , ConstructorTypeRegistry
    , constructorMetadataFromScheme
    , constructorScheme
    , constructorFieldTypes
    , constructorFieldTypeAt
    , globalConstructorTypeRegistryRef
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef)
import qualified Data.Set as Set
import Data.Set (Set)
import System.IO.Unsafe (unsafePerformIO)

import IHC.AST (Name)
import IHC.TypeAST (Scheme(..), Type(..), applySubst, freeTyVars, emptySubst, tyArrowArgs)
import IHC.TypeUnify (unify)

-- | A constructor is identified by its declaring module as well as its name.
-- Bare constructor names are not globally unique in Haskell programs.
data ConstructorIdentity = ConstructorIdentity
    { ciOwner :: !Name
    , ciName  :: !Name
    } deriving (Eq, Ord, Show)

-- | Full declaration metadata for one constructor.
--
-- @ctmResultType@ is deliberately a 'Type', rather than merely the data type
-- head: a GADT result such as @G [b]@ carries the equality that determines the
-- field type.  Variables which do not occur in the result are recorded as
-- existential and currently rejected by lookup; runtime dispatch has no
-- evidence with which to determine them.  Data-family constructors are also
-- rejected until family-instance ownership is represented in the key.
data ConstructorTypeMetadata = ConstructorTypeMetadata
    { ctmIdentity        :: !ConstructorIdentity
    , ctmQuantifiedVars  :: ![Name]
    , ctmExistentialVars :: ![Name]
    , ctmResultType      :: !Type
    , ctmFieldTypes      :: ![Type]
    , ctmDataFamily      :: !Bool
    } deriving (Eq, Show)

type ConstructorTypeRegistry = Map ConstructorIdentity ConstructorTypeMetadata

{-# NOINLINE globalConstructorTypeRegistryRef #-}
globalConstructorTypeRegistryRef :: IORef ConstructorTypeRegistry
globalConstructorTypeRegistryRef = unsafePerformIO (newIORef Map.empty)

-- | Convert a constructor signature produced by the source scanner into the
-- runtime-neutral metadata contract.  Variables quantified by the signature
-- but absent from its result are marked existential and therefore fail closed
-- at lookup until evidence support is implemented.
constructorMetadataFromScheme
    :: ConstructorIdentity -> Bool -> Scheme -> Maybe ConstructorTypeMetadata
constructorMetadataFromScheme identity' isDataFamily (Scheme vars preds body)
    | not (null preds) = Nothing -- constructor contexts/evidence not modelled
    | otherwise =
        let (fields, result) = tyArrowArgs body
            resultVars = freeTyVars result
            existentials = filter (`Set.notMember` resultVars) vars
        in Just ConstructorTypeMetadata
            { ctmIdentity = identity'
            , ctmQuantifiedVars = vars
            , ctmExistentialVars = existentials
            , ctmResultType = result
            , ctmFieldTypes = fields
            , ctmDataFamily = isDataFamily
            }

-- | Recover the constructor's complete source signature for local inference.
-- Qualified names are resolved only against their stated module.  A bare name
-- prefers a constructor declared by the lexical owner and otherwise succeeds
-- only when exactly one loaded module declares it.  This conservative rule is
-- important: choosing the first of two same-named constructors can silently
-- select a class instance for the wrong field type.
constructorScheme
    :: ConstructorTypeRegistry -> Maybe Name -> Name -> Maybe Scheme
constructorScheme registry requestedOwner requestedCtor = do
    identity' <- resolveIdentity
    metadata <- Map.lookup identity' registry
    if ctmIdentity metadata /= identity'
        || ctmDataFamily metadata
        || not (null (ctmExistentialVars metadata))
      then Nothing
      else Just (Scheme
            (ctmQuantifiedVars metadata)
            []
            (foldr TyArrow (ctmResultType metadata) (ctmFieldTypes metadata)))
  where
    (ctorQualifier, bareCtor) = splitQualified requestedCtor
    localIdentity = (`ConstructorIdentity` bareCtor) <$> requestedOwner
    candidates =
        [ identity'
        | identity' <- Map.keys registry
        , ciName identity' == bareCtor
        ]
    resolveIdentity = case ctorQualifier of
        Just qualifier -> Just (ConstructorIdentity qualifier bareCtor)
        Nothing -> case localIdentity of
            Just identity' | Map.member identity' registry -> Just identity'
            _ -> case candidates of
                [identity'] -> Just identity'
                _ -> Nothing

constructorFieldTypes
    :: ConstructorTypeRegistry
    -> Maybe Name                 -- ^ owner when known from lexical resolution
    -> Name                       -- ^ bare or module-qualified constructor
    -> Type
    -> Maybe [Type]
constructorFieldTypes registry requestedOwner requestedCtor expected = do
    (owner, bareCtor) <- resolveIdentity
    metadata <- Map.lookup (ConstructorIdentity owner bareCtor) registry
    if ctmIdentity metadata /= ConstructorIdentity owner bareCtor
        || ctmDataFamily metadata || not (null (ctmExistentialVars metadata))
      then Nothing
      else do
        -- Reject malformed/incomplete scanner output rather than allowing the
        -- unifier to invent meanings for undeclared variables.
        let declared = Set.fromList (ctmQuantifiedVars metadata)
            used = Set.unions
                (freeTyVars (ctmResultType metadata)
                    : map freeTyVars (ctmFieldTypes metadata))
        if not (used `Set.isSubsetOf` declared)
          then Nothing
          else do
            let avoid = Set.unions
                    (freeTyVars expected : freeTyVars (ctmResultType metadata)
                        : map freeTyVars (ctmFieldTypes metadata))
                freshVars = takeFresh (length (ctmQuantifiedVars metadata)) avoid
                freshSubst = Map.fromList
                    (zip (ctmQuantifiedVars metadata) (map TyVar freshVars))
                freshResult = applySubst freshSubst (ctmResultType metadata)
                freshFields = map (applySubst freshSubst) (ctmFieldTypes metadata)
            declaredResult <- normalizeResultHead owner freshResult
            expectedResult <- normalizeResultHead owner expected
            subst <- either (const Nothing) Just
                (unify emptySubst declaredResult expectedResult)
            pure (map (applySubst subst) freshFields)
  where
    (ctorQualifier, bareCtor0) = splitQualified requestedCtor

    resolveIdentity = case (requestedOwner, ctorQualifier) of
        (Just owner, Just qualifier)
            | owner == qualifier -> Just (owner, bareCtor0)
            | otherwise -> Nothing
        (Just owner, Nothing) -> Just (owner, bareCtor0)
        (Nothing, Just owner) -> Just (owner, bareCtor0)
        (Nothing, Nothing) -> case
            [ ciOwner identity'
            | identity' <- Map.keys registry
            , ciName identity' == bareCtor0
            ] of
              [owner] -> Just (owner, bareCtor0)
              _       -> Nothing

constructorFieldTypeAt
    :: ConstructorTypeRegistry -> Maybe Name -> Name -> Int -> Type -> Maybe Type
constructorFieldTypeAt registry owner ctor fieldIndex expected = do
    fields <- constructorFieldTypes registry owner ctor expected
    if fieldIndex < 0 || fieldIndex >= length fields
        then Nothing
        else Just (fields !! fieldIndex)

-- Only erase a result-head qualifier when it is the metadata's actual owner.
-- Thus @A.G@ and @G@ can agree for A's constructor, while @B.G@ cannot.
normalizeResultHead :: Name -> Type -> Maybe Type
normalizeResultHead owner ty = case ty of
    TyCon name -> TyCon <$> normalizeName name
    TyApp f x  -> (`TyApp` x) <$> normalizeResultHead owner f
    _          -> Just ty
  where
    normalizeName name = case splitQualified name of
        (Nothing, bare) -> Just bare
        (Just qualifier, bare)
            | qualifier == owner -> Just bare
            | otherwise -> Nothing

splitQualified :: ByteString -> (Maybe ByteString, ByteString)
splitQualified name = case BC.elemIndexEnd '.' name of
    Just i | i > 0, i + 1 < BC.length name ->
        (Just (BC.take i name), BC.drop (i + 1) name)
    _ -> (Nothing, name)

takeFresh :: Int -> Set Name -> [Name]
takeFresh count avoid = take count
    [ candidate
    | i <- [(0 :: Int)..]
    , let candidate = BC.pack ("$ctor" <> show i)
    , Set.notMember candidate avoid
    ]
