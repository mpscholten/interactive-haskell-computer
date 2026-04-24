-- | Type inference state — type signatures, type synonyms, and the
-- set of declared class method names.
--
-- These previously lived in three top-level 'unsafePerformIO' IORef
-- CAFs so the scheduler (which populates them as modules load) and
-- the evaluator/elaborator (which consult them) could share state
-- without a compile-time module cycle.
--
-- The CAFs are gone.  The three refs now live on 'IHC.Runtime.IHCRuntime'
-- (fields 'rtTypeSigs', 'rtTypeSynonyms', 'rtClassMethodNames'); this
-- module exposes a single helper to seed the canonical class-method
-- signatures.
module IHC.TypeGlobals
    ( seedBuiltinClassMethodSigs
    ) where

import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import IHC.Runtime (IHCRuntime(..))
import IHC.TypeAST (Scheme(..), Pred(..), Type(..))

-- | Seed the sig registry with a small table of canonical class
-- method signatures.  Our top-level sig scanner ('scanTypeSigs') only
-- picks up sigs at column 1; class method sigs live INSIDE class
-- declarations (@class Foo a where method :: ...@) and need the
-- class's constraint prepended to become useful standalone schemes.
-- A proper fix extends 'scanClassDecls' to capture method sigs + the
-- class's type parameter; this seed is a temporary shortcut.  Called
-- at REPL / program start so the elaborator has something to
-- instantiate for @pure@, @return@, etc.
seedBuiltinClassMethodSigs :: IHCRuntime -> IO ()
seedBuiltinClassMethodSigs rt = do
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
    modifyIORef' (rtTypeSigs rt) $ \s ->
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
    modifyIORef' (rtClassMethodNames rt) $ Set.union $ Set.fromList
        [ BC.pack "pure"
        , BC.pack "return"
        , BC.pack "mempty"
        , BC.pack "minBound"
        , BC.pack "maxBound"
        ]
