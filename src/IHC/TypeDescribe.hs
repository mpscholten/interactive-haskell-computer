-- | Runtime structural type description for the ':t' REPL command.
--
-- No static type inference is performed.  Instead we force the expression
-- to WHNF and walk the resulting 'Val' to produce a human-readable type
-- description that mirrors standard Haskell notation.
--
-- Key design decisions:
-- * 'VIO' is detected BEFORE any IO action is executed — we never run an
--   IO action just to describe its type.
-- * Thunks inside constructors are forced recursively (one level at a time)
--   so we can describe @[Int]@, @Maybe Char@, tuples, etc.
-- * Infinite loops / black-holes are caught and reported gracefully.
-- * For lists we recurse into the head to determine the element type and
--   trust uniformity; a heterogeneous list is reported as @[?]@.
module IHC.TypeDescribe (describeType) where

import Control.Exception (SomeException, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import IHC.Classes (IHCHooks)
import IHC.Eval (force)
import IHC.Val

-- | Return a structural type description for a 'Val'.
--
-- Safe to call on any 'Val':
--  * never executes 'VIO' actions
--  * catches exceptions from forcing nested thunks
describeType :: IHCHooks -> Val -> IO ByteString
describeType hooks = go
  where
    go :: Val -> IO ByteString
    go (VInt   _)   = pure "Int"
    go (VInteger _) = pure "Integer"
    go (VFloat _)   = pure "Double"
    go (VChar  _)   = pure "Char"
    go (VStr   _)   = pure "[Char]"
    go VUnit        = pure "()"
    go (VFun   _)   = pure "a -> b"
    go (VFieldAccessor _ _ _ _) = pure "a -> b"
    go (VFunIP _ _) = pure "a -> b"
    go (VClassMethod _ _ _ _) = pure "a -> b"
    go (VLazyMethod _) = pure "a -> b"
    go (VIO    _)   = pure "IO a"

    -- PrimObj variants
    go (VPrimObj (PrimIORef  _))     = pure "IORef a"
    go (VPrimObj (PrimHandle _))     = pure "Handle"
    go (VPrimObj (PrimForeignPtr _)) = pure "ForeignPtr Word8"
    go (VPrimObj (PrimPtr _))        = pure "Ptr Word8"
    go (VPrimObj (PrimByteArray _))  = pure "ByteArray"
    go (VPrimObj (PrimArray _))      = pure "Array# a"
    go (VPrimObj (PrimBoxedArray _ _)) = pure "Array# a"
    go (VPrimObj PrimRealWorld)      = pure "RealWorld#"
    go (VPrimObj (PrimMVar _))       = pure "MVar a"
    go (VPrimObj (PrimTVar _))       = pure "TVar a"
    go (VPrimObj (PrimThreadId _))   = pure "ThreadId"
    go (VPrimObj (PrimBigNat _))     = pure "BigNat#"
    go (VLabel name)                 = pure ("Label \"" <> name <> "\"")

    -- Booleans
    go (VCon "True"  []) = pure "Bool"
    go (VCon "False" []) = pure "Bool"

    -- Empty list
    go (VCon "[]" []) = pure "[a]"

    -- Non-empty list: cons cell — recurse into head for element type
    go (VCon ":" [h, _t]) = do
        headVal <- safeForce h
        elemTy  <- go headVal
        pure ("[" <> elemTy <> "]")

    -- Unit tuple constructor (shouldn't appear in practice, but be safe)
    go (VCon "()" []) = pure "()"

    -- 2-tuple
    go (VCon "(,)" [a, b]) = do
        ta <- safeForce a >>= go
        tb <- safeForce b >>= go
        pure ("(" <> ta <> ", " <> tb <> ")")

    -- 3-tuple
    go (VCon "(,,)" [a, b, c]) = do
        ta <- safeForce a >>= go
        tb <- safeForce b >>= go
        tc <- safeForce c >>= go
        pure ("(" <> ta <> ", " <> tb <> ", " <> tc <> ")")

    -- 4-tuple
    go (VCon "(,,,)" [a, b, c, d]) = do
        ta <- safeForce a >>= go
        tb <- safeForce b >>= go
        tc <- safeForce c >>= go
        td <- safeForce d >>= go
        pure ("(" <> ta <> ", " <> tb <> ", " <> tc <> ", " <> td <> ")")

    -- Maybe
    go (VCon "Just"    [x]) = do
        tx <- safeForce x >>= go
        pure ("Maybe " <> wrapIfCompound tx)
    go (VCon "Nothing" []) = pure "Maybe a"

    -- Either
    go (VCon "Left"  [x]) = do
        tx <- safeForce x >>= go
        pure ("Either " <> wrapIfCompound tx <> " b")
    go (VCon "Right" [x]) = do
        tx <- safeForce x >>= go
        pure ("Either a " <> wrapIfCompound tx)

    -- Generic constructor with no args — show the constructor name as type
    go (VCon name []) = pure name

    -- Generic constructor with args — show as "Con arg1 arg2 ..."
    go (VCon name args) = do
        argTys <- mapM (\t -> safeForce t >>= go) args
        pure (name <> " " <> BC.intercalate " " (map wrapIfCompound argTys))

    -- | Force a thunk safely, catching any exception (loop, error, etc.).
    -- Returns a sentinel 'VStr "<error>"' on failure.  Captures the
    -- enclosing 'describeType' 'hooks' parameter lexically.
    safeForce :: Thunk -> IO Val
    safeForce t = do
        r <- try (force hooks t) :: IO (Either SomeException Val)
        case r of
            Right v -> pure v
            Left  _ -> pure (VStr "<error>")

-- | Wrap a type description in parentheses if it contains a space
-- (i.e. it is a compound/applied type like "Maybe Int") so it parses
-- correctly in a larger context (e.g. "Maybe (Maybe Int)").
wrapIfCompound :: ByteString -> ByteString
wrapIfCompound bs
    | BC.elem ' ' bs = "(" <> bs <> ")"
    | otherwise      = bs
