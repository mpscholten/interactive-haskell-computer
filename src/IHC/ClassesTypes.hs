-- | Pure types for the class-dispatch machinery.
--
-- Split out of 'IHC.Classes' so that 'IHC.Runtime' can hold
-- 'ClassRegistry', 'ScanHook', and 'EnvFallbackHook' fields on
-- 'IHCRuntime' without importing 'IHC.Classes' — which would cycle
-- back once 'IHC.Classes' imports 'IHC.Runtime' to project rt fields.
module IHC.ClassesTypes
    ( MethodTable
    , ClassRegistry
    , ScanHook
    , EnvFallbackHook
    , InstanceScopeRef
    , newClassRegistry
    , normalizeTyTag
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)

import IHC.Val (Thunk, Val)

-- | A per-instance method dictionary: the set of method implementations
-- for a specific @(ClassName, TypeTag)@ pair.
type MethodTable = Map ByteString Val

-- | The per-run class registry. Maps @(ClassName, [TypeTag])@ to a
-- method-name/value table.  The tag list supports multi-parameter type
-- classes (MPTC).  Single-parameter classes use a 1-element list.
type ClassRegistry = IORef (Map (ByteString, [ByteString]) MethodTable)

-- | A hook that records newly-scanned module names during lazy instance
-- discovery.
type ScanHook = ByteString -> IO ()

-- | A hook consulted by 'IHC.Eval.eval' on 'EVar' miss.  Given a name,
-- produces a thunk from a module the evaluator hasn't yet seen.
type EnvFallbackHook = ByteString -> IO (Maybe Thunk)

-- | The instance-scope ref: the accumulated set of module names whose
-- instance decls are currently in scope for dispatch (Haskell 2010
-- §4.3.2).
type InstanceScopeRef = IORef (Set ByteString)

newClassRegistry :: IO ClassRegistry
newClassRegistry = newIORef Map.empty

-- | Canonicalise a 'TypeRep' string so that @Maybe Int@, @(Maybe Int)@,
-- @\"Int\"@, and @'x'@ all dispatch to the same 'MethodTable' entry.
-- Pure helper; lives here so 'IHC.Scan' can consume it without cycling
-- through 'IHC.Classes' -> 'IHC.Runtime'.
normalizeTyTag :: ByteString -> ByteString
normalizeTyTag bs0 = stripQuotes (trimSpace (stripParens bs0))
  where
    trimSpace s =
        BC.dropWhile isSpace (BC.reverse (BC.dropWhile isSpace (BC.reverse s)))
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

    stripParens s
        | BC.length s >= 2
        , BC.head s == '('
        , BC.last s == ')'    = stripParens (trimSpace (BC.init (BC.tail s)))
        | otherwise           = s

    stripQuotes s
        | BC.length s >= 2
        , BC.head s == '"'
        , BC.last s == '"'    = BC.init (BC.tail s)
        | BC.length s >= 2
        , BC.head s == '\''
        , BC.last s == '\''   = BC.init (BC.tail s)
        | otherwise           = s
