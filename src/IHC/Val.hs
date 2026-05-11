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
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Int (Int64)
import Data.Word (Word8)
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Numeric.Natural (Natural)
import System.IO (Handle)

import IHC.AST (Expr, Name)

--------------------------------------------------------------------------------
-- Values
--------------------------------------------------------------------------------

-- | Weak-head-normal-form values. Lazy data appears as 'Thunk's
-- inside a 'VCon'.
data Val
    = VInt   !Int64
    | VInteger !Integer                -- arbitrary-precision Integer (A.3);
                                       -- produced by 'ELit (LInteger _)'.
                                       -- Today only the print path handles
                                       -- VInteger directly; arithmetic
                                       -- between VInteger and VInt errors
                                       -- pending the elaborator-driven
                                       -- fromInteger-insertion path.
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
    -- | Unforced instance method body. Produced by 'evalMethodWithLazy'
    -- during 'registerInstancesFrom' and forced by the class-method
    -- dispatcher ('classMethodDispatcher') when the method is looked up.
    -- Deferring the force to dispatch time avoids the env-snapshot bug
    -- where rewritten FQNs had no slot at registration time because
    -- 'discoverInModule' hadn't yet populated the target module's
    -- 'lmBodies'.
    | VLazyMethod !Thunk

-- | Opaque host-side objects surfaced to the interpreter. Not
-- user-inspectable — programs pass them through primops only
-- ('readIORef', 'hClose', …).
data PrimObj
    -- The slot holds a 'Thunk' (not a 'Val') so 'newIORef $ error msg'
    -- behaves correctly: in real Haskell, the IORef stores a thunk that
    -- raises only when read, never at construction time.  Our previous
    -- 'IORef Val' representation forced the value at @newIORef@, so warp's
    -- @keepAliveRef <- newIORef $ error \"keepAliveRef not filled\"@
    -- raised in 'processRequest' before the response handler ran.
    -- Reads force the thunk on demand; writes install a fresh
    -- (already-evaluated) thunk via 'newWHNFThunk'.
    = PrimIORef  !(IORef Thunk)
    | PrimHandle !Handle
    -- Phase 2.8: low-level memory objects for ByteString / ForeignPtr support.
    | PrimForeignPtr !(ForeignPtr Word8)
    | PrimPtr        !(Ptr Word8)
    | PrimByteArray  !(IORef ByteString)   -- mutable byte array backed by ByteString
    | PrimArray      !(IORef [Thunk])       -- boxed Array#/MutableArray# cells
    | PrimRealWorld                        -- zero-size phantom token
    -- Boxed mutable/frozen array, the runtime backing for
    -- @MutableArray# s a@ and @Array# a@.  We share one representation
    -- because @unsafeFreezeArray#@ is zero-cost in GHC — it just
    -- relabels the tag.  The 'IntMap' is dense-ish (populated by
    -- @newArray#@ with the same initial thunk at every index) and
    -- updated in place by @writeArray#@.  Length is tracked
    -- separately because callers that only read a slice don't care
    -- about the underlying map size.
    | PrimBoxedArray !Int !(IORef (Map Int Thunk))
    -- Phase 2.10a: concurrency primitives backed by host GHC RTS.
    | PrimMVar     !(MVar Val)
    | PrimTVar     !(TVar Val)
    | PrimThreadId !ThreadId
    -- ghc-bignum 'BigNat#' runtime representation (Phase 1 of the
    -- full source-loaded Integer roadmap, see
    -- @plans/full-ghc-bignum-source-load.md@).  ghc-bignum's
    -- canonical layout is @type BigNat# = WordArray#@ — a ByteArray#
    -- of Word#-sized limbs in little-endian order with the high
    -- limb non-zero.  We deliberately choose host 'Natural'
    -- (unsigned arbitrary precision) instead, because the entire
    -- BigNat# primop suite (Phase 2) is intentionally host-shimmed
    -- as thin wrappers over Natural arithmetic.  Sign-direction
    -- (IP vs IN) is encoded in the surrounding 'Integer' constructor;
    -- the 'PrimBigNat' itself is always an unsigned magnitude.
    | PrimBigNat !Natural

showValForDebug :: Val -> String
showValForDebug (VInt n)    = show n
showValForDebug (VInteger n) = show n
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
showValForDebug (VPrimObj (PrimArray _))      = "<MutableArray#>"
showValForDebug (VPrimObj (PrimBoxedArray _ _)) = "<BoxedArray#>"
showValForDebug (VPrimObj PrimRealWorld)      = "<RealWorld#>"
showValForDebug (VPrimObj (PrimMVar _))       = "<MVar>"
showValForDebug (VPrimObj (PrimTVar _))       = "<TVar>"
showValForDebug (VPrimObj (PrimThreadId _))   = "<ThreadId>"
showValForDebug (VPrimObj (PrimBigNat n))     = "<BigNat# " <> show n <> ">"
showValForDebug (VLabel name)                = "#" <> BC.unpack name
showValForDebug (VClassMethod m _ tags _)     =
    "<classMethod " <> BC.unpack m
    <> (if null tags then "" else " @" <> show (map BC.unpack tags)) <> ">"
showValForDebug (VLazyMethod _)               = "<lazyMethod>"

--------------------------------------------------------------------------------
-- Thunks
--------------------------------------------------------------------------------

type Thunk = IORef ThunkState

data ThunkState
    = Unevaluated !Closure
    | Evaluated   !Val
    | BlackHole  !String                -- entered, not yet returned
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

-- | Phase 2.18: switched to 'Data.HashMap.Strict' for the runtime
-- environment.  Closure evaluation is dominated by 'EVar' lookups,
-- where 'Data.Map' was costing log(n) ByteString compares per hit
-- (~50 bytes per fully-qualified name × ~8 levels deep for a 200-key
-- env = ~400 byte ops per lookup).  Profiling warp's hello-world
-- showed the interpreter saturated in 'Data.ByteString.compareBytes'
-- under @schedule@.  HashMap.lookup costs one hash + at most one
-- ByteString eq, eliminating the log factor entirely.
type Env = HashMap Name Thunk

emptyEnv :: Env
emptyEnv = HashMap.empty

extendEnv :: Name -> Thunk -> Env -> Env
extendEnv = HashMap.insert

-- | Strict left fold so the chain of intermediate 'HashMap.insert'
-- thunks doesn't accumulate. Called after every successful pattern
-- match (Eval.tryAlts), every let-binding allocation, every module-
-- import qualified-slot install, etc. — small per call, but in the
-- hot path. Semantics match 'foldr' for our callers because every
-- caller builds 'kvs' from a unique-name source ('zip names slots'
-- for pattern bindings; 'qualPairs' for imports) so no internal
-- duplicates exist where left-vs-right fold direction would matter.
extendEnvMany :: [(Name, Thunk)] -> Env -> Env
extendEnvMany kvs env = foldl' (\e (k, v) -> HashMap.insert k v e) env kvs

lookupEnv :: Name -> Env -> Maybe Thunk
lookupEnv = HashMap.lookup

--------------------------------------------------------------------------------
-- Runtime failures
--------------------------------------------------------------------------------

data LoopException = LoopException String deriving Show
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
