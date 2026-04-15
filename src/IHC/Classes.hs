-- | Type class registry for Phase 2.3 dictionary-passing implementation.
--
-- Type classes are implemented via runtime dispatch: a global
-- 'ClassRegistry' maps (ClassName, TypeTag) -> [method_Val] where each
-- slot corresponds to one method of the class (in declaration order).
--
-- 'typeTagOf' inspects a 'Val' and returns a stable string tag that
-- identifies its runtime type for dispatch purposes.
module IHC.Classes
    ( ClassRegistry
    , newClassRegistry
    , registerInstance
    , lookupInstance
    , typeTagOf
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import IHC.Val

-- | The global class registry. Maps (ClassName, TypeTag) to an ordered
-- list of method values (one per method, in class-declaration order).
type ClassRegistry = IORef (Map (ByteString, ByteString) [Val])

newClassRegistry :: IO ClassRegistry
newClassRegistry = newIORef Map.empty

-- | Register a dict (method list) for a (class, type-tag) pair.
-- Overwrites any previously registered instance (last write wins).
registerInstance :: ClassRegistry -> ByteString -> ByteString -> [Val] -> IO ()
registerInstance reg className typeTag methods =
    modifyIORef' reg (Map.insert (className, typeTag) methods)

-- | Look up the method list for a given (class, type-tag) pair.
lookupInstance :: ClassRegistry -> ByteString -> ByteString -> IO (Maybe [Val])
lookupInstance reg className typeTag = do
    m <- readIORef reg
    pure (Map.lookup (className, typeTag) m)

-- | Return a stable string tag for the runtime type of a value.
-- Used by dispatch builtins to find the right class instance.
typeTagOf :: Val -> ByteString
typeTagOf (VInt _)    = BC.pack "Int"
typeTagOf (VChar _)   = BC.pack "Char"
typeTagOf (VStr _)    = BC.pack "String"   -- transitional VStr
typeTagOf VUnit       = BC.pack "()"
typeTagOf (VCon "[]" _) = BC.pack "[]"
typeTagOf (VCon ":" _)  = BC.pack "[]"
typeTagOf (VCon "True"  _) = BC.pack "Bool"
typeTagOf (VCon "False" _) = BC.pack "Bool"
typeTagOf (VCon "(,)" _)   = BC.pack "(,)"
typeTagOf (VCon "(,,)" _)  = BC.pack "(,,)"
typeTagOf (VCon n _)    = n
typeTagOf (VFun _)      = BC.pack "<function>"
typeTagOf (VIO _)       = BC.pack "<IO>"
typeTagOf (VPrimObj _)  = BC.pack "<PrimObj>"
