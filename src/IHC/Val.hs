-- | Runtime value + thunk + environment types.
--
-- Operations on these (force, eval, apply) live in 'IHC.Eval' so we
-- avoid a module cycle. This module is purely the data definitions
-- shared between the evaluator and any code that constructs or
-- inspects runtime values.
module IHC.Val
    ( -- * Values
      Val(..)
    , PrimObj(..)
    , showValForDebug
      -- * Thunks
    , Thunk
    , ThunkState(..)
    , Closure(..)
    , newThunk
    , newThunkIP
    , newWHNFThunk
    , newLazyBuiltinThunk
      -- * Environments
    , Env
    , emptyEnv
    , extendEnv
    , extendEnvMany
    , lookupEnv
      -- * Implicit-parameter map (Phase 3.6)
    , ImplicitParamMap
    , emptyIPMap
    , extendIPMap
    , lookupIPMap
      -- * Failures
    , LoopException(..)
    , PatternMatchFail(..)
    , IhcException(..)
    ) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.STM (TVar)
import Control.Exception (Exception)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import System.IO (Handle)

import IHC.AST (Expr, Name)

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

-- | Weak-head-normal-form values. Lazy data appears as 'Thunk's
-- inside a 'VCon'.
data Val
    = VInt   !Int64
    | VFloat !Double                   -- Float/Double (Phase 2.9+)
    | VChar  !Char                     -- single character (Phase 2.2+)
    | VStr   !ByteString               -- raw bytes — transitional, some
                                       -- builtins still produce these;
                                       -- user-visible strings are [Char]
    | VFun  !(Thunk -> IO Val)         -- single-argument closure (builtins)
    -- Phase 3.6: user-defined lambda with implicit-param support.
    -- The function receives the caller's ImplicitParamMap so it can
    -- merge (lexically-bound ?params win over caller's).
    | VFunIP !ImplicitParamMap !(ImplicitParamMap -> Thunk -> IO Val)
    | VCon  !Name ![Thunk]             -- saturated constructor (Phase 2.1+)
    | VUnit                            -- () — IO result of putStrLn etc.
    | VIO   !(IO Val)                  -- suspended IO action (Phase 2.4)
    | VPrimObj !PrimObj                -- opaque host object (Phase 2.4)
    | VLabel   !ByteString             -- #name OverloadedLabels value (Phase 3.5)
    -- | Unsaturated class-method dispatcher. Accumulates TypeApplications
    -- type args as it flows through 'ETyApp' nodes, then performs
    -- multi-key lookup in the 'ClassRegistry' when applied to a value
    -- argument. The list of 'ByteString' tags is type-arg tags in source
    -- order (e.g. @setField \@\"name\" \@User \@String@ accumulates
    -- @[\"name\",\"User\",\"String\"]@ — quotes stripped by the
    -- normaliser). The 'Thunk -> IO Val' callback does the actual
    -- lookup+apply at the point the dispatcher is applied to a runtime
    -- argument; the 'Name' and 'Int' payloads are informational (method
    -- name + class-slot) kept for error messages.
    | VClassMethod !Name !Int ![ByteString] !([ByteString] -> Thunk -> IO Val)

-- | Opaque host-side objects surfaced to the interpreter. Not
-- user-inspectable — programs pass them through primops only
-- ('readIORef', 'hClose', …).
data PrimObj
    = PrimIORef  !(IORef Val)
    | PrimHandle !Handle
    -- Phase 2.8: low-level memory objects for ByteString / ForeignPtr support.
    | PrimForeignPtr !(ForeignPtr Word8)
    | PrimPtr        !(Ptr Word8)
    | PrimByteArray  !(IORef ByteString)   -- mutable byte array backed by ByteString
    | PrimRealWorld                        -- zero-size phantom token
    -- Phase 2.10a: concurrency primitives backed by host GHC RTS.
    | PrimMVar     !(MVar Val)
    | PrimTVar     !(TVar Val)
    | PrimThreadId !ThreadId

showValForDebug :: Val -> String
showValForDebug (VInt n)    = show n
showValForDebug (VFloat d)  = show d
showValForDebug (VChar c)   = show c
showValForDebug (VStr s)    = show (BC.unpack s)
showValForDebug (VFun _)      = "<function>"
showValForDebug (VFunIP _ _)  = "<function>"
showValForDebug (VCon n _)  = "<" <> BC.unpack n <> "...>"
showValForDebug VUnit       = "()"
showValForDebug (VIO _)     = "<IO>"
showValForDebug (VPrimObj (PrimIORef _))       = "<IORef>"
showValForDebug (VPrimObj (PrimHandle _))     = "<Handle>"
showValForDebug (VPrimObj (PrimForeignPtr _)) = "<ForeignPtr>"
showValForDebug (VPrimObj (PrimPtr _))        = "<Ptr>"
showValForDebug (VPrimObj (PrimByteArray _))  = "<MutableByteArray>"
showValForDebug (VPrimObj PrimRealWorld)      = "<RealWorld#>"
showValForDebug (VPrimObj (PrimMVar _))       = "<MVar>"
showValForDebug (VPrimObj (PrimTVar _))       = "<TVar>"
showValForDebug (VPrimObj (PrimThreadId _))   = "<ThreadId>"
showValForDebug (VLabel name)                = "#" <> BC.unpack name
showValForDebug (VClassMethod m _ tags _)     =
    "<classMethod " <> BC.unpack m
    <> (if null tags then "" else " @" <> show (map BC.unpack tags)) <> ">"

--------------------------------------------------------------------------------
-- Thunks
--------------------------------------------------------------------------------

type Thunk = IORef ThunkState

data ThunkState
    = Unevaluated !Closure
    | Evaluated   !Val
    | BlackHole                         -- entered, not yet returned
    -- | Lazy-init for builtins: a host @IO Val@ action that produces the
    -- builtin's value on first force. Used by 'IHC.Builtins.builtinEnv'
    -- so startup doesn't pay the cost of materialising every primop,
    -- dispatch wrapper, and Typeable dict up-front. Semantics match
    -- 'Unevaluated' — the value is produced once and memoised — but we
    -- avoid building a Closure+Env+Expr around a host function that
    -- doesn't come from source.
    | LazyBuiltin !(IO Val)

-- | Map of implicit-parameter names (@?x@) to thunks, threaded through
-- closures for lexical scoping. Empty for most code; non-empty only when
-- an @ImplicitParams@ binding is in scope.
type ImplicitParamMap = Map Name Thunk

emptyIPMap :: ImplicitParamMap
emptyIPMap = Map.empty

extendIPMap :: Name -> Thunk -> ImplicitParamMap -> ImplicitParamMap
extendIPMap = Map.insert

lookupIPMap :: Name -> ImplicitParamMap -> Maybe Thunk
lookupIPMap = Map.lookup

-- | A closure captures both the regular environment and the implicit-param
-- map at the point of its creation (lexical scoping for both).
data Closure = Closure !Env !ImplicitParamMap !Expr

newThunk :: Env -> Expr -> IO Thunk
newThunk env expr = newIORef (Unevaluated (Closure env emptyIPMap expr))

-- | Like 'newThunk' but also captures the current implicit-param map.
newThunkIP :: Env -> ImplicitParamMap -> Expr -> IO Thunk
newThunkIP env ipm expr = newIORef (Unevaluated (Closure env ipm expr))

newWHNFThunk :: Val -> IO Thunk
newWHNFThunk v = newIORef (Evaluated v)

-- | Make a thunk whose evaluation runs a host @IO Val@ action. Used by
-- 'IHC.Builtins.builtinEnv' to defer materialisation of primop values
-- until they're actually referenced. The action runs at most once —
-- 'IHC.Eval.force' writes the produced 'Val' back with 'Evaluated'.
newLazyBuiltinThunk :: IO Val -> IO Thunk
newLazyBuiltinThunk mkV = newIORef (LazyBuiltin mkV)

--------------------------------------------------------------------------------
-- Environments
--------------------------------------------------------------------------------

type Env = Map Name Thunk

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Name -> Thunk -> Env -> Env
extendEnv = Map.insert

extendEnvMany :: [(Name, Thunk)] -> Env -> Env
extendEnvMany kvs env = foldr (\(k, v) e -> Map.insert k v e) env kvs

lookupEnv :: Name -> Env -> Maybe Thunk
lookupEnv = Map.lookup

--------------------------------------------------------------------------------
-- Runtime failures
--------------------------------------------------------------------------------

data LoopException = LoopException deriving Show
instance Exception LoopException

newtype PatternMatchFail = PatternMatchFail String deriving Show
instance Exception PatternMatchFail

-- | Phase 2.10a: wrapper for exceptions thrown by interpreter-level
-- 'throwIO' / 'throw'. The 'Thunk' holds the Val-level exception value;
-- 'ByteString' is a display string for 'show'.
data IhcException = IhcException !ByteString !(IORef ThunkState)
instance Show IhcException where
    show (IhcException msg _) = "IhcException: " <> BC.unpack msg
instance Exception IhcException
