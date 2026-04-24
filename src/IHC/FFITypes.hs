-- | Pure data types that describe a scanned @foreign import ccall@
-- declaration.  Split out of 'IHC.FFI' so that modules which only need
-- the shapes (e.g. 'IHC.Scan', 'IHC.Runtime') don't pull in the
-- dispatch machinery that depends on 'IHC.Eval' — which in turn would
-- create an import cycle once the evaluator is threaded with
-- 'IHC.Runtime.IHCRuntime'.
module IHC.FFITypes
    ( FFIType(..)
    , Safety(..)
    , CallConv(..)
    , ForeignDecl(..)
    ) where

import Data.ByteString (ByteString)

-- | A subset of the C type vocabulary that appears in real Hackage
-- @foreign import ccall@ declarations.  The parser (@scanForeignImports@)
-- maps user-visible names like @CInt@, @CSize@, @Ptr@, @CString@ to one
-- of these tags.
data FFIType
    = FFIVoid
    | FFIInt
    | FFIUInt
    | FFILong
    | FFIULong
    | FFISize
    | FFIChar
    | FFIUChar
    | FFIInt8
    | FFIInt16
    | FFIInt32
    | FFIInt64
    | FFIWord8
    | FFIWord16
    | FFIWord32
    | FFIWord64
    | FFIFloat
    | FFIDouble
    | FFIPtr      !FFIType   -- @Ptr a@ — payload type is carried for clarity
    | FFIFunPtr   !FFIType   -- @FunPtr a@
    | FFICString             -- @CString@ == @Ptr CChar@, specialised to marshal ByteString
    deriving (Eq, Show)

data Safety = Safe | Unsafe | Interruptible
    deriving (Eq, Show)

data CallConv = CCall | CApi | StdCall | Prim
    deriving (Eq, Show)

-- | The scanned form of a @foreign import@ declaration.
data ForeignDecl = ForeignDecl
    { fdName     :: !ByteString     -- ^ Haskell name, e.g. @"c_strlen"@
    , fdSymbol   :: !ByteString     -- ^ C symbol, e.g. @"strlen"@.  For
                                    --   address-of imports, the leading
                                    --   @&@ is already stripped.
    , fdSafety   :: !Safety
    , fdCallConv :: !CallConv
    , fdArgTypes :: ![FFIType]
    , fdRetType  :: !FFIType        -- ^ type inside IO (or pure)
    , fdIsIO     :: !Bool           -- ^ does the result sit inside @IO@?
    , fdIsAddrOf :: !Bool           -- ^ @foreign import ccall "&sym" p :: Ptr T@
                                    --   — take the address of a C global
                                    --   symbol rather than call it.  No
                                    --   args, return is always a pointer.
    } deriving (Eq, Show)
