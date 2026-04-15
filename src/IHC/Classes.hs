-- | Type class registry for Phase 2.3 dictionary-passing implementation.
--
-- Type classes are implemented via runtime dispatch: a global
-- 'ClassRegistry' maps (ClassName, TypeTag) -> [method_Val] where each
-- slot corresponds to one method of the class (in declaration order).
--
-- 'typeTagOf' inspects a 'Val' and returns a stable string tag that
-- identifies its runtime type for dispatch purposes.
--
-- Phase 2.9.5: TypeRep infrastructure.
-- TypeRep is represented as VCon "TypeRep" [tyConThunk, argsThunk]
-- where tyConThunk = VCon "TyCon" [nameThunk] and argsThunk = list of
-- sub-TypeReps. This is structural, compositional, and equality is
-- just VCon equality.
module IHC.Classes
    ( ClassRegistry
    , newClassRegistry
    , registerInstance
    , lookupInstance
    , typeTagOf
      -- * Phase 2.9.5: TypeRep helpers
    , mkTyCon
    , mkTypeRep
    , mkTypeRepApp
    , builtinTypeRep
    , typeRepEq
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.IO.Unsafe (unsafePerformIO)

import IHC.Val

--------------------------------------------------------------------------------
-- Phase 2.9.5: TypeRep construction helpers
--------------------------------------------------------------------------------

-- | Build a TyCon value: VCon "TyCon" [nameThunk].
-- The name is the type constructor name as a [Char] list.
mkTyCon :: ByteString -> IO Val
mkTyCon name = do
    nameT <- stringToThunk (BC.unpack name)
    pure (VCon "TyCon" [nameT])

-- | Build a TypeRep for a nullary type constructor (no applied args).
-- VCon "TypeRep" [tyConThunk, emptyListThunk]
mkTypeRep :: ByteString -> IO Val
mkTypeRep name = do
    tyConV <- mkTyCon name
    tyConT <- newWHNFThunk tyConV
    nilT   <- newWHNFThunk (VCon "[]" [])
    pure (VCon "TypeRep" [tyConT, nilT])

-- | Build a TypeRep for an applied type: f applied to args.
-- VCon "TypeRep" [tyConThunk, argsListThunk]
-- where args are sub-TypeReps for each applied argument.
mkTypeRepApp :: ByteString -> [Val] -> IO Val
mkTypeRepApp name argReps = do
    tyConV <- mkTyCon name
    tyConT <- newWHNFThunk tyConV
    argTs  <- mapM newWHNFThunk argReps
    -- Build a [TypeRep] list.
    listV  <- valListFromThunks argTs
    listT  <- newWHNFThunk listV
    pure (VCon "TypeRep" [tyConT, listT])

-- Build a Haskell list Val from a list of thunks.
valListFromThunks :: [Thunk] -> IO Val
valListFromThunks []     = pure (VCon "[]" [])
valListFromThunks (t:ts) = do
    rest  <- valListFromThunks ts
    restT <- newWHNFThunk rest
    pure (VCon ":" [t, restT])

-- | Build a [Char] list Val from a String.
stringToThunk :: String -> IO Thunk
stringToThunk s = do
    v <- stringToVal s
    newWHNFThunk v

stringToVal :: String -> IO Val
stringToVal []     = pure (VCon "[]" [])
stringToVal (c:cs) = do
    rest  <- stringToVal cs
    restT <- newWHNFThunk rest
    charT <- newWHNFThunk (VChar c)
    pure (VCon ":" [charT, restT])

-- | TypeRep for a built-in primitive type by name.
-- Convenience: builds a nullary TypeRep for Int, Char, Bool, etc.
builtinTypeRep :: ByteString -> IO Val
builtinTypeRep = mkTypeRep

-- | Structural equality on TypeRep values.
-- Two TypeReps are equal iff their TyCon names and all argument TypeReps
-- are equal. This mirrors Eq TypeRep in GHC's implementation.
typeRepEq :: Val -> Val -> IO Bool
typeRepEq (VCon "TypeRep" [tc1, args1]) (VCon "TypeRep" [tc2, args2]) = do
    b1 <- tyConEq tc1 tc2
    if not b1 then pure False
    else do
        v1 <- readIORef args1 >>= forceThunkState
        v2 <- readIORef args2 >>= forceThunkState
        typeRepListEq v1 v2
typeRepEq _ _ = pure False

tyConEq :: Thunk -> Thunk -> IO Bool
tyConEq t1 t2 = do
    v1 <- readIORef t1 >>= forceThunkState
    v2 <- readIORef t2 >>= forceThunkState
    case (v1, v2) of
        (VCon "TyCon" [n1], VCon "TyCon" [n2]) -> strEq n1 n2
        _ -> pure False

strEq :: Thunk -> Thunk -> IO Bool
strEq t1 t2 = do
    v1 <- readIORef t1 >>= forceThunkState
    v2 <- readIORef t2 >>= forceThunkState
    pure (valToString v1 == valToString v2)

valToString :: Val -> String
valToString (VCon "[]" [])    = []
valToString (VCon ":" [h, t]) =
    -- TypeRep name strings are always fully evaluated; safe to peek.
    case readThunkPure h of
        VChar c -> c : valToString (readThunkPure t)
        _       -> []
valToString (VStr bs) = BC.unpack bs
valToString _         = []

-- | Read a thunk's value without an IO context.
-- Safe only for WHNF-evaluated thunks (which TypeRep name strings always are).
readThunkPure :: Thunk -> Val
readThunkPure t =
    case unsafePerformIO (readIORef t) of
        Evaluated v  -> v
        _            -> VStr (BC.pack "<thunk>")

typeRepListEq :: Val -> Val -> IO Bool
typeRepListEq (VCon "[]" []) (VCon "[]" []) = pure True
typeRepListEq (VCon ":" [h1,t1]) (VCon ":" [h2,t2]) = do
    v1 <- readIORef h1 >>= forceThunkState
    v2 <- readIORef h2 >>= forceThunkState
    b  <- typeRepEq v1 v2
    if not b then pure False
    else do
        r1 <- readIORef t1 >>= forceThunkState
        r2 <- readIORef t2 >>= forceThunkState
        typeRepListEq r1 r2
typeRepListEq _ _ = pure False

forceThunkState :: ThunkState -> IO Val
forceThunkState (Evaluated v) = pure v
forceThunkState (Unevaluated _) = pure (VStr (BC.pack "<unevaluated>"))
forceThunkState BlackHole = pure (VStr (BC.pack "<blackhole>"))

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
typeTagOf (VFloat _)  = BC.pack "Double"
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
typeTagOf (VFunIP _ _)  = BC.pack "<function>"
typeTagOf (VIO _)       = BC.pack "<IO>"
typeTagOf (VPrimObj (PrimIORef _))       = BC.pack "<IORef>"
typeTagOf (VPrimObj (PrimHandle _))      = BC.pack "<Handle>"
typeTagOf (VPrimObj (PrimForeignPtr _))  = BC.pack "<ForeignPtr>"
typeTagOf (VPrimObj (PrimPtr _))         = BC.pack "<Ptr>"
typeTagOf (VPrimObj (PrimByteArray _))   = BC.pack "<MutableByteArray>"
typeTagOf (VPrimObj PrimRealWorld)       = BC.pack "<RealWorld#>"
typeTagOf (VPrimObj (PrimMVar _))        = BC.pack "<MVar>"
typeTagOf (VPrimObj (PrimTVar _))        = BC.pack "<TVar>"
typeTagOf (VPrimObj (PrimThreadId _))    = BC.pack "<ThreadId>"
typeTagOf (VLabel _)                     = BC.pack "Label"
