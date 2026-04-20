-- | Global IORef-backed registries for top-level type signatures and
-- type synonyms.  Populated by the scheduler each time a module is
-- loaded; consulted by 'IHC.Elaborate' for on-demand type inference.
--
-- Split into its own module so both the scheduler (which populates)
-- and the evaluator (which triggers inference) can access the refs
-- without introducing a cycle.
module IHC.TypeGlobals
    ( globalTypeSigsRef
    , globalTypeSynonymsRef
    , seedBuiltinClassMethodSigs
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)

import IHC.TypeAST (Scheme(..), Pred(..), Type(..))

-- | Flat union of every loaded module's top-level type signatures.
-- Used by 'IHC.Elaborate' to look up a binding's type when inference
-- needs it.  Last-writer-wins on name collisions across modules
-- (uncommon in practice — sigs are module-scoped in source).
{-# NOINLINE globalTypeSigsRef #-}
globalTypeSigsRef :: IORef (Map ByteString Scheme)
globalTypeSigsRef = unsafePerformIO (newIORef Map.empty)

-- | Flat union of type synonym declarations (@type Name v1 v2 … =
-- RHS@).  One-hop expansion is all the elaborator does today:
-- @State s a@ → @StateT s Identity a@ is expanded once during
-- unification.  Multi-level chains would need a fixed-point pass.
{-# NOINLINE globalTypeSynonymsRef #-}
globalTypeSynonymsRef :: IORef (Map ByteString (Int, Type))
globalTypeSynonymsRef = unsafePerformIO (newIORef Map.empty)

-- | Seed the sig registry with a small table of canonical class
-- method signatures.  Our top-level sig scanner ('scanTypeSigs') only
-- picks up sigs at column 1; class method sigs live INSIDE class
-- declarations (@class Foo a where method :: ...@) and need the
-- class's constraint prepended to become useful standalone schemes.
-- A proper fix extends 'scanClassDecls' to capture method sigs + the
-- class's type parameter; this seed is a temporary shortcut.  Called
-- at REPL / program start so the elaborator has something to
-- instantiate for @pure@, @return@, etc.
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
