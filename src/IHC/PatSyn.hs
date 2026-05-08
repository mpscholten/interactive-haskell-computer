-- | Global registry of pattern synonym declarations.
--
-- A pattern synonym is a top-level decl of the form
--
-- > pattern Name p1 p2 ... <- body   -- unidirectional
-- > pattern Name p1 p2 ... =  body   -- bidirectional (we only use the
-- >                                     "match" direction here)
--
-- When pattern matching encounters @PCon \"Name\" args@, we look up
-- @Name@ here.  If found, we substitute the supplied @args@ for the
-- parameter names @p1, p2, ...@ in the registered @body@ pattern, and
-- match the result against the value.
--
-- The registry is populated by 'IHC.Scan.scanPatternSynonyms' when a
-- module is loaded (see 'buildLoadedModule').
module IHC.PatSyn
    ( PatSyn (..)
    , globalPatSynRef
    , registerPatSyns
    , lookupPatSyn
    , substPat
    ) where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)

import IHC.AST (Pat (..), Name)

data PatSyn = PatSyn
    { psParams :: ![Name]
    , psBody   :: !Pat
    }
    deriving Show

{-# NOINLINE globalPatSynRef #-}
globalPatSynRef :: IORef (Map ByteString PatSyn)
globalPatSynRef = unsafePerformIO (newIORef Map.empty)

-- | Bulk-register synonyms (used at module load time).  Last-writer-wins
-- across modules — pattern synonyms are normally module-scoped so this
-- only matters for re-declarations across scan passes (idempotent).
registerPatSyns :: [(ByteString, PatSyn)] -> IO ()
registerPatSyns ps =
    modifyIORef' globalPatSynRef $ \m ->
        foldr (\(k, v) -> Map.insert k v) m ps

lookupPatSyn :: ByteString -> IO (Maybe PatSyn)
lookupPatSyn n = do
    m <- readIORef globalPatSynRef
    pure (Map.lookup n m)

-- | Substitute pattern parameters with the supplied argument patterns.
-- Used when expanding a pattern-synonym occurrence:
--
-- @pattern Head x \<- (x:_)@  with  @PCon \"Head\" [PVar \"y\"]@
--   ==> @PCon \":\" [PVar \"y\", PWild]@
--
-- Each occurrence of @PVar paramName@ inside the body pattern is
-- replaced by the corresponding argument pattern.  Other variable
-- references (introduced by @as@ patterns, etc.) are left untouched —
-- they only become bound in scope of the user's match site.
substPat :: Map Name Pat -> Pat -> Pat
substPat sub = go
  where
    go p = case p of
        PVar n         -> case Map.lookup n sub of
                              Just q  -> q
                              Nothing -> p
        PCon n ps      -> PCon n (map go ps)
        PAs n p'       -> PAs n (go p')
        PBang p'       -> PBang (go p')
        PIrref p'      -> PIrref (go p')
        PTuple ps      -> PTuple (map go ps)
        PRecord n fps  -> PRecord n [(f, go q) | (f, q) <- fps]
        PRecordWild n  -> PRecordWild n
        PView e p'     -> PView e (go p')
        PWild          -> PWild
        PLit l         -> PLit l
