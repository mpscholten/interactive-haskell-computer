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
    , newWHNFThunk
      -- * Environments
    , Env
    , emptyEnv
    , extendEnv
    , extendEnvMany
    , lookupEnv
      -- * Failures
    , LoopException(..)
    , PatternMatchFail(..)
    ) where

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
    = VInt  !Int64
    | VChar !Char                      -- single character (Phase 2.2+)
    | VStr  !ByteString                -- raw bytes — transitional, some
                                       -- builtins still produce these;
                                       -- user-visible strings are [Char]
    | VFun  !(Thunk -> IO Val)         -- single-argument closure
    | VCon  !Name ![Thunk]             -- saturated constructor (Phase 2.1+)
    | VUnit                            -- () — IO result of putStrLn etc.
    | VIO   !(IO Val)                  -- suspended IO action (Phase 2.4)
    | VPrimObj !PrimObj                -- opaque host object (Phase 2.4)

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

showValForDebug :: Val -> String
showValForDebug (VInt n)    = show n
showValForDebug (VChar c)   = show c
showValForDebug (VStr s)    = show (BC.unpack s)
showValForDebug (VFun _)    = "<function>"
showValForDebug (VCon n _)  = "<" <> BC.unpack n <> "...>"
showValForDebug VUnit       = "()"
showValForDebug (VIO _)     = "<IO>"
showValForDebug (VPrimObj (PrimIORef _))       = "<IORef>"
showValForDebug (VPrimObj (PrimHandle _))     = "<Handle>"
showValForDebug (VPrimObj (PrimForeignPtr _)) = "<ForeignPtr>"
showValForDebug (VPrimObj (PrimPtr _))        = "<Ptr>"
showValForDebug (VPrimObj (PrimByteArray _))  = "<MutableByteArray>"
showValForDebug (VPrimObj PrimRealWorld)      = "<RealWorld#>"

--------------------------------------------------------------------------------
-- Thunks
--------------------------------------------------------------------------------

type Thunk = IORef ThunkState

data ThunkState
    = Unevaluated !Closure
    | Evaluated   !Val
    | BlackHole                         -- entered, not yet returned

data Closure = Closure !Env !Expr

newThunk :: Env -> Expr -> IO Thunk
newThunk env expr = newIORef (Unevaluated (Closure env expr))

newWHNFThunk :: Val -> IO Thunk
newWHNFThunk v = newIORef (Evaluated v)

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
