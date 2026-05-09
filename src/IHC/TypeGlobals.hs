-- | Global IORef-backed registries for top-level type signatures and
-- type synonyms.  Populated by the scheduler each time a module is
-- loaded; consulted by 'IHC.Elaborate' for on-demand type inference.
--
-- Split into its own module so both the scheduler (which populates)
-- and the evaluator (which triggers inference) can access the refs
-- without introducing a cycle.
module IHC.TypeGlobals
    ( -- * Legacy registry refs (now field accessors over 'legacyTypeState')
      globalTypeSigsRef
    , globalTypeSynonymsRef
    , globalClassMethodNamesRef
    , globalMethodClassRef
      -- * Bundle that backs the four legacy registries
    , LegacyTypeState(..)
    , legacyTypeState
    , seedBuiltinClassMethodSigs
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

import IHC.TypeAST (Scheme(..), Pred(..), Type(..))

-- | Bundle of the four module-level type registries that the
-- elaborator and scheduler share: top-level signatures, type
-- synonyms, the set of names declared as class methods, and the
-- method → declaring-class map.
--
-- Allocated once via the 'legacyTypeState' CAF below.  The
-- @global*Ref@ accessors that the rest of the codebase still uses
-- are now field-projections on this single record, so the four
-- separate @unsafePerformIO + IORef + NOINLINE@ globals collapse
-- into one allocation.
data LegacyTypeState = LegacyTypeState
    { ltsTypeSigs           :: !(IORef (Map ByteString Scheme))
    , ltsTypeSynonyms       :: !(IORef (Map ByteString (Int, Type)))
    , ltsClassMethodNames   :: !(IORef (Set ByteString))
    , ltsMethodClass        :: !(IORef (Map ByteString [ByteString]))
    }

-- | One-shot allocation of the four type-registry IORefs.
{-# NOINLINE legacyTypeState #-}
legacyTypeState :: LegacyTypeState
legacyTypeState = unsafePerformIO $ do
    sigs        <- newIORef Map.empty
    synonyms    <- newIORef Map.empty
    classNames  <- newIORef Set.empty
    methodClass <- newIORef Map.empty
    pure LegacyTypeState
        { ltsTypeSigs         = sigs
        , ltsTypeSynonyms     = synonyms
        , ltsClassMethodNames = classNames
        , ltsMethodClass      = methodClass
        }

-- | Flat union of every loaded module's top-level type signatures.
-- Used by 'IHC.Elaborate' to look up a binding's type when inference
-- needs it.  Last-writer-wins on name collisions across modules
-- (uncommon in practice — sigs are module-scoped in source).
globalTypeSigsRef :: IORef (Map ByteString Scheme)
globalTypeSigsRef = ltsTypeSigs legacyTypeState

-- | Flat union of type synonym declarations (@type Name v1 v2 … =
-- RHS@).  One-hop expansion is all the elaborator does today:
-- @State s a@ → @StateT s Identity a@ is expanded once during
-- unification.  Multi-level chains would need a fixed-point pass.
globalTypeSynonymsRef :: IORef (Map ByteString (Int, Type))
globalTypeSynonymsRef = ltsTypeSynonyms legacyTypeState

-- | Names that really appear as methods in a @class C where m1 :: ...@
-- declaration somewhere in the loaded modules.  Populated by the
-- scheduler's @buildClassMethodEnv@ and consulted by
-- @IHC.Elaborate.classMethodHint@.
--
-- Without this, @classMethodHint@ would treat any top-level binding
-- with a one-class-pred signature whose tyvar appears in the body as a
-- class method — which catches honest functions like
-- @array :: Ix i => (i, i) -> [(i, e)] -> Array i e@ and routes calls
-- to them through the class dispatcher ("no instance of Ix for type
-- `(,)`").
globalClassMethodNamesRef :: IORef (Set ByteString)
globalClassMethodNamesRef = ltsClassMethodNames legacyTypeState

-- | Method-name → declaring-class-names. Used by the env-fallback to
-- lazily synthesise a class-method dispatcher when a bare reference
-- (@abs@, @negate@, …) misses the env. Populated alongside
-- 'globalClassMethodNamesRef' by the scheduler's @buildClassMethodEnv@.
-- A method may appear in multiple classes (rare, e.g. user code defining
-- a custom @Foldable@-shadowing class); we keep the list in
-- registration order and the fallback tries each class until one's
-- dispatcher resolves.
globalMethodClassRef :: IORef (Map ByteString [ByteString])
globalMethodClassRef = ltsMethodClass legacyTypeState

-- | Seed the sig registry with a small table of canonical class
-- method signatures.  Our top-level sig scanner ('scanTypeSigs') only
-- picks up sigs at column 1; class method sigs live INSIDE class
-- declarations (@class Foo a where method :: ...@) and need the
-- class's constraint prepended to become useful standalone schemes.
-- A proper fix extends 'scanClassDecls' to capture method sigs + the
-- class's type parameter; this seed is a temporary shortcut.  Called
-- at REPL / program start so the elaborator has something to
-- instantiate for @pure@, @return@, etc.
--
-- Also seeds 'globalClassMethodNamesRef' with the same names so the
-- elaborator's @classMethodHint@ recognises them as actual class
-- methods even before @scanClassDecls@ is run for the loaded modules.
seedBuiltinClassMethodSigs :: IO ()
seedBuiltinClassMethodSigs = do
    let a = BC.pack "a"
        f = BC.pack "f"
        m = BC.pack "m"
        -- pure :: Applicative f => a -> f a
        pureSig = Scheme [f, a]
                    [Pred (BC.pack "Applicative") (TyVar f)]
                    (TyArrow (TyVar a) (TyApp (TyVar f) (TyVar a)))
        -- return :: Monad m => a -> m a
        returnSig = Scheme [m, a]
                    [Pred (BC.pack "Monad") (TyVar m)]
                    (TyArrow (TyVar a) (TyApp (TyVar m) (TyVar a)))
        -- mempty :: Monoid a => a
        memptySig = Scheme [a]
                    [Pred (BC.pack "Monoid") (TyVar a)]
                    (TyVar a)
        -- minBound :: Bounded a => a
        minBoundSig = Scheme [a]
                    [Pred (BC.pack "Bounded") (TyVar a)]
                    (TyVar a)
        -- maxBound :: Bounded a => a
        maxBoundSig = minBoundSig
    modifyIORef' globalTypeSigsRef $ \s ->
        Map.unions
            [ Map.fromList
                [ (BC.pack "pure",     pureSig)
                , (BC.pack "return",   returnSig)
                , (BC.pack "mempty",   memptySig)
                , (BC.pack "minBound", minBoundSig)
                , (BC.pack "maxBound", maxBoundSig)
                ]
            , s  -- existing sigs win over seed (scanner-provided sigs preferred)
            ]
    -- Mirror the seed names into the class-method whitelist so the
    -- elaborator recognises them as actual class methods even before
    -- @scanClassDecls@ runs for the user's modules.
    modifyIORef' globalClassMethodNamesRef $ Set.union $ Set.fromList $ map BC.pack
        [ "pure"
        , "return"
        , "mempty"
        , "minBound"
        , "maxBound"
        ]
    -- Seed method→class for the builtin numeric/enum/float classes so
    -- the env-fallback can synthesise a 'classMethodDispatcher' on demand
    -- when bare references like @abs@ or @negate@ resolve before the
    -- declaring module's @scanClassDecls@ has populated the registry
    -- — and to cover the methods the scanner currently misses (multi-name
    -- sigs like @(+), (-), (*) :: a -> a -> a@ and single-name sigs
    -- preceded by Haddock comments inside class blocks).
    modifyIORef' globalMethodClassRef $ Map.unionWith (\a b -> a ++ filter (`notElem` a) b) $
        Map.fromListWith (++) $ map (\(m, c) -> (BC.pack m, [BC.pack c]))
            -- Num
            [ ("+",          "Num")
            , ("-",          "Num")
            , ("*",          "Num")
            , ("negate",     "Num")
            , ("abs",        "Num")
            , ("signum",     "Num")
            , ("fromInteger","Num")
            -- Enum
            , ("succ",       "Enum")
            , ("pred",       "Enum")
            , ("toEnum",     "Enum")
            , ("fromEnum",   "Enum")
            -- Bounded
            , ("minBound",   "Bounded")
            , ("maxBound",   "Bounded")
            -- Integral (mod / div / quotRem / divMod don't need seeding —
            -- the registry handles them after Integral source-loads via
            -- triggerCoreInstanceLoad / registerGlobalLoadedModule.  These
            -- are seeded for first-call locality so bare references like
            -- @17 \`quot\` 5@ in the REPL hit a 1-step lookup instead of
            -- the 3-step probe→load→discover cascade.)
            , ("quot",       "Integral")
            , ("rem",        "Integral")
            , ("toInteger",  "Integral")
            -- Fractional
            , ("/",          "Fractional")
            , ("recip",      "Fractional")
            , ("fromRational","Fractional")
            -- Floating
            , ("pi",         "Floating")
            , ("exp",        "Floating")
            , ("log",        "Floating")
            , ("sqrt",       "Floating")
            , ("**",         "Floating")
            , ("logBase",    "Floating")
            , ("sin",        "Floating")
            , ("cos",        "Floating")
            , ("tan",        "Floating")
            , ("asin",       "Floating")
            , ("acos",       "Floating")
            , ("atan",       "Floating")
            , ("sinh",       "Floating")
            , ("cosh",       "Floating")
            , ("tanh",       "Floating")
            , ("asinh",      "Floating")
            , ("acosh",      "Floating")
            , ("atanh",      "Floating")
            -- RealFrac
            , ("properFraction","RealFrac")
            , ("truncate",   "RealFrac")
            , ("round",      "RealFrac")
            , ("ceiling",    "RealFrac")
            , ("floor",      "RealFrac")
            -- Foldable
            , ("length",     "Foldable")
            -- Applicative / Monad seeds (already in env via builtins, but
            -- registering the class lets future env-fallbacks dispatch).
            , ("pure",       "Applicative")
            , ("return",     "Monad")
            -- Monoid / Semigroup
            , ("mempty",     "Monoid")
            , ("mappend",    "Monoid")
            , ("mconcat",    "Monoid")
            , ("<>",         "Semigroup")
            ]
