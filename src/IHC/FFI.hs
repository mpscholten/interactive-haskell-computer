-- | Generic @foreign import ccall@ dispatcher.
--
-- Replaces per-function host builtins: any @foreign import ccall "symbol"
-- name :: T1 -> ... -> IO Tn@ parsed from source produces a 'Val' that, when
-- applied to its arguments, dispatches the real libc\/libpq\/whatever symbol
-- via libffi.
--
-- This is the standard interpreter answer to FFI: the library of dispatch
-- types (@ffi_type_sint@, @ffi_type_pointer@, …) is how GHC's own FFI
-- ultimately works.  Using it here means a source-loaded Hackage library
-- gets its FFI imports \"for free\" — the interpreter never needs a shim.
--
-- Scope of the first commit:
--
--   * 'FFIType' covers the common scalar types (signed\/unsigned 8\/16\/32\/64,
--     CInt\/CLong\/CSize, CFloat\/CDouble), 'Ptr', and 'CString'.
--   * Variadic calls ('printf' family), @foreign import ccall \"wrapper\"@
--     (Haskell→C→Haskell callbacks) and struct pass-by-value are NOT
--     handled — they require @ffi_prep_cif_var@ \/ @ffi_closure@ and are
--     explicitly out of scope here.  Deferred.
--
-- Symbol resolution: 'resolveSymbol' opens @libSystem@ on first use
-- (Darwin — @libSystem.B.dylib@ covers libc, libm, and the POSIX surface).
-- 'registerLibrary' lets the scheduler add per-package @extra-libraries@
-- entries (e.g. @libpq.dylib@ for @hasql@).
module IHC.FFI
    ( -- * Types
      FFIType(..)
    , Safety(..)
    , CallConv(..)
    , ForeignDecl(..)
      -- * Symbol resolution
    , registerLibrary
    , registerCbitsDylibs
    , resolveSymbol
      -- * Cross-run reset (wired into 'IHC.Scheduler.resetPerRunGlobals')
    , clearOpenLibs
    , clearSymbolCache
      -- Exposed only so 'IHC.MemDebug' can size them for the
      -- @IHC_MEM_DEBUG@ probe.
    , openLibs
    , symbolCache
      -- * Dispatch
    , callForeign
    , makeForeignVal
    ) where

import Control.Exception (throwIO, catch, SomeException)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Word (Word32)
import Foreign.LibFFI
    ( Arg, callFFI
    , argCInt, argCUInt, argCLong, argCULong
    , argInt8, argInt16, argInt32, argInt64
    , argWord8, argWord16, argWord32, argWord64
    , argCFloat, argCDouble, argCSize
    , argCChar, argCUChar, argPtr, argConstByteString
    , retVoid, retCInt, retCUInt, retCLong, retCULong
    , retInt8, retInt16, retInt32, retInt64
    , retWord8, retWord16, retWord32, retWord64
    , retCFloat, retCDouble, retCSize
    , retCChar, retCUChar, retPtr, retCString
    )
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, FunPtr, nullPtr, nullFunPtr, castPtr, castFunPtrToPtr)
import qualified GHC.Conc.Sync as HostConc
import System.Directory (doesDirectoryExist, listDirectory)
import qualified System.Environment as Env
import System.FilePath ((</>), takeExtension)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Posix.DynamicLinker as DL

import IHC.Classes (legacyHooks)
import IHC.Eval (force)
import IHC.Val

foreign import ccall "&RtsFlags"
    hsRtsFlagsPtr :: Ptr ()

foreign import ccall "&enabled_capabilities"
    hsEnabledCapabilitiesPtr :: Ptr Word32

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

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
    | FFIThreadId           -- @ThreadId#@, only valid for RTS-special leaves
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

--------------------------------------------------------------------------------
-- Symbol resolution
--------------------------------------------------------------------------------

-- | Libraries opened via 'registerLibrary'. 'resolveSymbol' walks them
-- in insertion order, then falls back to the process-global default
-- (@libSystem@ on Darwin, which covers libc \/ libm \/ POSIX).
openLibs :: IORef [(ByteString, DL.DL)]
openLibs = unsafePerformIO (newIORef [])
{-# NOINLINE openLibs #-}

-- | Resolved @symbol → FunPtr@ cache. Shared across every call site so
-- that repeated invocations of a builtin-backed symbol only pay the
-- @dlsym@ cost once.
symbolCache :: IORef (Map ByteString (FunPtr ()))
symbolCache = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE symbolCache #-}

-- | Reset the opened-shared-library list between 'loadProgramFromSource'
-- runs.  Mirrors the 'IHC.Builtins.clearCtorIndex' precedent.  Safe:
-- 'registerCbitsDylibs' re-opens every @IHC_CBITS_DIR@ library at the
-- start of the next run, and @dlopen@ on an already-resident library is
-- a cheap refcount bump.  In practice this list is bounded (registration
-- is idempotent per path), so this is reset-surface hygiene rather than
-- the OOM driver — see the @IHC_MEM_DEBUG@ findings.
clearOpenLibs :: IO ()
clearOpenLibs = writeIORef openLibs []

-- | Reset the @symbol → FunPtr@ cache between runs.  Mirrors
-- 'IHC.Builtins.clearCtorIndex'.  Bounded by the symbol universe so
-- this is precautionary (kept uniform with 'clearOpenLibs'); symbols
-- are re-resolved lazily via 'resolveSymbol' on next use.
clearSymbolCache :: IO ()
clearSymbolCache = writeIORef symbolCache Map.empty

-- | Open a shared library by name (or absolute path) and remember it for
-- later symbol lookups.  Idempotent: opening the same library twice is
-- a no-op.  Silently swallows @dlopen@ failures so that a package's
-- declared @extra-libraries:@ doesn't abort interpreter startup when a
-- user hasn't installed the dev package for it — the error only shows
-- up later when an actual FFI symbol from that library is invoked.
-- | Auto-discover per-package cbits shared libraries.  The nix build
-- emits one @libhs<pkg>-cbits.dylib@ (Darwin) or @libhs<pkg>-cbits.so@
-- (Linux) per Hackage source package that declares @c-sources:@ in its
-- @.cabal@, and exposes the directory containing them via the
-- @IHC_CBITS_DIR@ environment variable.  This function dlopens every
-- shared library in that directory at interpreter startup so that
-- @foreign import ccall@ declarations resolve via
-- @dlsym(RTLD_DEFAULT, …)@ without any package-specific wiring.
--
-- We accept both @.dylib@ and @.so@ rather than CPP-gating on the host
-- OS because the IHC binary itself is portable Haskell — the only thing
-- that varies is what file extension the nix derivation produced for the
-- *current* host.  Globbing both means a single source tree builds &
-- runs on macOS and Linux without #ifdef.
--
-- Running outside a dev shell (@IHC_CBITS_DIR@ unset, or directory
-- absent) is silent — user's code that actually exercises one of the
-- missing symbols will error at FFI-call time with a clearer message
-- than a nix derivation complaint would give.
registerCbitsDylibs :: IO ()
registerCbitsDylibs = do
    mDir <- Env.lookupEnv "IHC_CBITS_DIR"
    case mDir of
        Nothing  -> pure ()
        Just dir -> do
            exists <- doesDirectoryExist dir
            if not exists
                then pure ()
                else do
                    entries <- listDirectory dir
                    let isShared e = takeExtension e == ".dylib"
                                  || takeExtension e == ".so"
                        dylibs = [ dir </> e | e <- entries, isShared e ]
                    mapM_ (\p -> registerLibrary (BC.pack p)) dylibs

registerLibrary :: ByteString -> IO ()
registerLibrary name = do
    opened <- readIORef openLibs
    case lookup name opened of
        Just _  -> pure ()
        Nothing -> do
            r <- try' (DL.dlopen (BC.unpack name) [DL.RTLD_LAZY, DL.RTLD_GLOBAL])
            case r of
                Right dl -> modifyIORef' openLibs ((name, dl) :)
                Left  _  -> pure ()
  where
    try' :: IO a -> IO (Either SomeException a)
    try' io = (Right <$> io) `catch` (pure . Left)

-- | Look up @sym@ in every registered library, then in the process
-- default.  Caches the result.  Returns 'Nothing' if no library
-- exposes the symbol.
resolveSymbol :: ByteString -> IO (Maybe (FunPtr ()))
resolveSymbol sym = do
    cache <- readIORef symbolCache
    case Map.lookup sym cache of
        Just p  -> pure (Just p)
        Nothing -> do
            mPtr <- findInLibs (symbolAlias sym)
            case mPtr of
              Just p  -> do
                  modifyIORef' symbolCache (Map.insert sym p)
                  pure (Just p)
              Nothing -> pure Nothing
  where
    symbolAlias s =
        Map.findWithDefault s s (Map.fromList
            [ ("hsnet_getaddrinfo",  "getaddrinfo")
            , ("hsnet_freeaddrinfo", "freeaddrinfo")
            , ("hsnet_getnameinfo",  "getnameinfo")
            ])

    findInLibs lookupSym = do
        libs <- readIORef openLibs
        tryEach lookupSym (map snd libs ++ [DL.Default])
    tryEach _ [] = pure Nothing
    tryEach lookupSym (h:hs) = do
        r <- try' (DL.dlsym h (BC.unpack lookupSym))
        case r of
            Right p | p /= nullFunPtr -> pure (Just p)
            _                         -> tryEach lookupSym hs

    try' :: IO a -> IO (Either SomeException a)
    try' io = (Right <$> io) `catch` (pure . Left)

--------------------------------------------------------------------------------
-- Marshalling
--------------------------------------------------------------------------------

-- | Convert a forced 'Val' plus a declared 'FFIType' into a libffi 'Arg'.
-- Everything that the interpreter stores as 'VInt' is a machine @Int64@;
-- we narrow to the declared ABI width here.  'VCon "Ptr" [_]' is unwrapped
-- by 'ptrFromVal' — the IHC runtime shape for 'Ptr' is either a raw
-- @PrimPtr@ (host-allocated) or a data-constructor @Ptr addr#@.
valToArg :: FFIType -> Val -> IO Arg
valToArg ty v = case ty of
    FFIInt    -> pure (argCInt   (fromIntegral (asInt v)))
    FFIUInt   -> pure (argCUInt  (fromIntegral (asInt v)))
    FFILong   -> pure (argCLong  (fromIntegral (asInt v)))
    FFIULong  -> pure (argCULong (fromIntegral (asInt v)))
    FFISize   -> pure (argCSize  (fromIntegral (asInt v)))
    FFIChar   -> pure (argCChar  (fromIntegral (asInt v)))
    FFIUChar  -> pure (argCUChar (fromIntegral (asInt v)))
    FFIInt8   -> pure (argInt8   (fromIntegral (asInt v)))
    FFIInt16  -> pure (argInt16  (fromIntegral (asInt v)))
    FFIInt32  -> pure (argInt32  (fromIntegral (asInt v)))
    FFIInt64  -> pure (argInt64  (fromIntegral (asInt v)))
    FFIWord8  -> pure (argWord8  (fromIntegral (asInt v)))
    FFIWord16 -> pure (argWord16 (fromIntegral (asInt v)))
    FFIWord32 -> pure (argWord32 (fromIntegral (asInt v)))
    FFIWord64 -> pure (argWord64 (fromIntegral (asInt v)))
    FFIFloat  -> pure (argCFloat  (realToFrac (asFloat v)))
    FFIDouble -> pure (argCDouble (realToFrac (asFloat v)))
    FFIThreadId -> throwIO (userError "IHC.FFI: ThreadId# requires an RTS-special foreign import")
    FFIVoid   -> throwIO (userError "IHC.FFI: void cannot appear as an argument")
    FFIPtr _      -> do p <- ptrFromVal v; pure (argPtr p)
    FFIFunPtr _   -> do p <- ptrFromVal v; pure (argPtr p)
    FFICString    -> case v of
        VPrimObj (PrimPtr _) -> do
            p <- ptrFromVal v
            pure (argPtr p)
        VCon "Ptr" [_] -> do
            p <- ptrFromVal v
            pure (argPtr p)
        VInt 0 -> pure (argPtr nullPtr)
        _ -> do
            bs <- byteStringFromVal v
            pure (argConstByteString bs)

asInt :: Val -> Int64
asInt (VInt n)    = n
asInt (VChar c)   = fromIntegral (fromEnum c)
asInt (VCon _ []) = 0    -- treat arity-0 constructors as 0 (e.g. False)
asInt other       = error ("IHC.FFI: expected numeric value, got " <> showValForDebug other)

asFloat :: Val -> Double
asFloat (VFloat d) = d
asFloat (VInt   n) = fromIntegral n
asFloat other      = error ("IHC.FFI: expected floating value, got " <> showValForDebug other)

-- | Unwrap a 'Val' to a raw host 'Ptr ()'.
--
--   * @VPrimObj (PrimPtr p)@ — already a raw host pointer.
--   * @VCon "Ptr" [addrT]@ — the source-level @Ptr addr#@ constructor.
--   * @VCon "ForeignPtr" [addrT, _]@ — use the address slot, caller
--     is responsible for @touchForeignPtr@ separately (not modelled yet;
--     fine for the MVP since the common uses pass the result promptly).
--   * @VInt 0@ — null pointer literal.
ptrFromVal :: Val -> IO (Ptr ())
ptrFromVal v = case v of
    VPrimObj (PrimPtr p)              -> pure (castPtr p)
    VCon "Ptr" [t]                    -> force legacyHooks t >>= ptrFromVal
    VCon "FunPtr" [t]                 -> force legacyHooks t >>= ptrFromVal
    VCon "ForeignPtr" (addrT : _)     -> force legacyHooks addrT >>= ptrFromVal
    VInt 0                            -> pure nullPtr
    -- ByteArray# / MutableByteArray# passed to C: our PrimByteArray
    -- wraps a ByteString.  Pin its bytes and hand out a raw pointer.
    -- The MallocForeignPtr (from the ByteString's slice) keeps the
    -- payload alive as long as the ByteString reference persists in
    -- the IORef.  unsafeUseAsCString + unsafePerformIO would allow
    -- the continuation to exit before the dispatch — instead we copy
    -- into a malloc buffer so the pointer remains valid past the
    -- callForeign boundary.  Leaks are acceptable for FFI arg
    -- marshalling (bounded by the number of actual calls).
    VPrimObj (PrimByteArray ref)      -> do
        bs <- readIORef ref
        p  <- BS.useAsCString bs (\cp -> do
                 let len = BS.length bs
                 buf <- mallocBytes len
                 BS.useAsCString bs (\src -> copyBytes buf src len)
                 pure (castPtr buf :: Ptr ()))
        pure p
    _ -> error ("IHC.FFI: expected Ptr/FunPtr, got " <> showValForDebug v)

-- | Pull out a 'ByteString' for 'CString' marshalling.
byteStringFromVal :: Val -> IO BS.ByteString
byteStringFromVal v = case v of
    VStr bs -> pure bs
    -- Source-level Haskell strings are [Char] (consed from VCon ":" / "[]").
    -- Build the byte stream by walking the list.
    VCon "[]" _   -> pure BS.empty
    VCon ":" _    -> consListToBS v
    _ -> error ("IHC.FFI: expected String/CString, got " <> showValForDebug v)
  where
    consListToBS :: Val -> IO BS.ByteString
    consListToBS = go []
      where
        go acc (VCon "[]" _) = pure (BS.pack (reverse acc))
        go acc (VCon ":" [hT, tT]) = do
            h <- force legacyHooks hT
            t <- force legacyHooks tT
            case h of
                VChar c -> go (fromIntegral (fromEnum c) : acc) t
                VInt  n -> go (fromIntegral n          : acc) t
                _       -> error ("IHC.FFI: non-Char/Int in String: " <> showValForDebug h)
        go _ other = error ("IHC.FFI: malformed [Char]: " <> showValForDebug other)

--------------------------------------------------------------------------------
-- Return-value unmarshalling
--------------------------------------------------------------------------------

-- | Call the resolved 'FunPtr' with the marshaled args, using the 'RetType'
-- for the declared return type, and wrap the result back into a 'Val'.
dispatchRet :: FFIType -> FunPtr () -> [Arg] -> IO Val
dispatchRet ty fp args = case ty of
    FFIVoid    -> callFFI fp retVoid   args >> pure VUnit
    FFIInt     -> (VInt . fromIntegral) <$> callFFI fp retCInt    args
    FFIUInt    -> (VInt . fromIntegral) <$> callFFI fp retCUInt   args
    FFILong    -> (VInt . fromIntegral) <$> callFFI fp retCLong   args
    FFIULong   -> (VInt . fromIntegral) <$> callFFI fp retCULong  args
    FFISize    -> (VInt . fromIntegral) <$> callFFI fp retCSize   args
    FFIChar    -> (VInt . fromIntegral) <$> callFFI fp retCChar   args
    FFIUChar   -> (VInt . fromIntegral) <$> callFFI fp retCUChar  args
    FFIInt8    -> (VInt . fromIntegral) <$> callFFI fp retInt8    args
    FFIInt16   -> (VInt . fromIntegral) <$> callFFI fp retInt16   args
    FFIInt32   -> (VInt . fromIntegral) <$> callFFI fp retInt32   args
    FFIInt64   -> (VInt . fromIntegral) <$> callFFI fp retInt64   args
    FFIWord8   -> (VInt . fromIntegral) <$> callFFI fp retWord8   args
    FFIWord16  -> (VInt . fromIntegral) <$> callFFI fp retWord16  args
    FFIWord32  -> (VInt . fromIntegral) <$> callFFI fp retWord32  args
    FFIWord64  -> (VInt . fromIntegral) <$> callFFI fp retWord64  args
    FFIFloat   -> (VFloat . realToFrac)  <$> callFFI fp retCFloat  args
    FFIDouble  -> (VFloat . realToFrac)  <$> callFFI fp retCDouble args
    FFIThreadId -> throwIO (userError "IHC.FFI: ThreadId# cannot be returned through generic libffi")
    FFIPtr _   -> do
        p <- callFFI fp (retPtr retCChar) args
        pure (VPrimObj (PrimPtr (castPtr p)))
    FFIFunPtr _ -> do
        p <- callFFI fp (retPtr retCChar) args
        pure (VPrimObj (PrimPtr (castPtr p)))
    FFICString -> do
        p <- callFFI fp retCString args
        if p == nullPtr
            then pure (VCon "[]" [])
            else do
                bs <- BS.packCString p
                bsToConsList bs

-- | Build a strict Haskell 'String' (an IHC cons-list of 'VChar') from
-- a 'ByteString'.  Materialises eagerly — trivial for the short strings
-- libc returns.  Replace with a lazy tail thunk if a workload ever
-- needs it.
bsToConsList :: BS.ByteString -> IO Val
bsToConsList bs = go (BS.unpack bs)
  where
    go []     = pure (VCon "[]" [])
    go (c:cs) = do
        hT  <- newWHNFThunk (VChar (toEnum (fromIntegral c)))
        tV  <- go cs
        tT  <- newWHNFThunk tV
        pure (VCon ":" [hT, tT])

--------------------------------------------------------------------------------
-- Entry points
--------------------------------------------------------------------------------

-- | Call the foreign symbol once, given the fully-saturated argument list.
-- Raises 'IhcException' (via 'throwIO') if the symbol cannot be resolved.
callForeign :: ForeignDecl -> [Val] -> IO Val
callForeign decl argVals
    | fdSymbol decl == BC.pack "rts_getThreadId"
    , [tidV] <- argVals = do
          tid <- extractThreadId tidV
          pure (VInteger (toInteger (HostConc.fromThreadId tid)))
    | fdSymbol decl == BC.pack "rtsSupportsBoundThreads"
    , null argVals =
          -- The host RTS symbol is not reliably visible through dlsym in the
          -- in-process test runner. Reporting false makes source-loaded
          -- threadDelay choose the delay# primop path, which is the actual
          -- non-event-manager primitive boundary IHC implements.
          pure (VInt 0)
    | otherwise = do
          mFp <- resolveSymbol (fdSymbol decl)
          case mFp of
              Nothing -> do
                  msgT <- newWHNFThunk (VStr (BC.pack "FFI symbol not found: " <> fdSymbol decl))
                  throwIO (IhcException
                      ("FFI: symbol '" <> fdSymbol decl <> "' not found in any loaded library")
                      msgT)
              Just fp -> do
                  argsFfi <- sequence (zipWith valToArg (fdArgTypes decl) argVals)
                  dispatchRet (fdRetType decl) fp argsFfi

extractThreadId :: Val -> IO HostConc.ThreadId
extractThreadId (VPrimObj (PrimThreadId tid)) = pure tid
extractThreadId (VCon "ThreadId" [innerT]) = do
    inner <- force legacyHooks innerT
    extractThreadId inner
extractThreadId other =
    error ("IHC.FFI: expected ThreadId#/ThreadId, got " <> showValForDebug other)

-- | Turn a 'ForeignDecl' into a runtime 'Val' that behaves like a
-- curried Haskell function of the declared arity.  Applying the final
-- argument produces:
--
--   * a 'VIO' action if the declared return type was wrapped in 'IO'
--     (the common case);
--   * the raw value directly if the import was declared pure.
--
-- Address-of imports (@foreign import ccall "&sym" p :: Ptr T@) take a
-- different shape: no args, no call — just the raw address of the C
-- symbol wrapped as a 'Ptr'.  They are used by Hackage libraries for
-- lookup tables (bytestring's hex/float tables), signal constants
-- (unix's @nocldstop@), and finalizer function pointers.
makeForeignVal :: ForeignDecl -> IO Val
makeForeignVal decl
    | fdIsAddrOf decl
    , fdSymbol decl == BC.pack "RtsFlags" =
          -- RTS-exclusive data symbol used by source-loaded GHC.RTS.Flags.
          -- It is not a Haskell module shim: the source foreign import still
          -- resolves through the generic FFI path, but in-process test
          -- runners do not expose this RTS data object through dlsym.
          pure (VPrimObj (PrimPtr (castPtr hsRtsFlagsPtr)))
    | fdIsAddrOf decl
    , fdSymbol decl == BC.pack "enabled_capabilities" =
          -- RTS-exclusive data symbol used by source-loaded
          -- GHC.Internal.Conc.Sync.getNumCapabilities.
          let p = castPtr hsEnabledCapabilitiesPtr
          in do
              markTypedHostPtr p (BC.pack "Word32")
              pure (VPrimObj (PrimPtr p))
    | fdIsAddrOf decl = do
          mFp <- resolveSymbol (fdSymbol decl)
          case mFp of
              Nothing -> do
                  msgT <- newWHNFThunk (VStr (BC.pack "FFI symbol not found (addr): " <> fdSymbol decl))
                  throwIO (IhcException
                      ("FFI: symbol '" <> fdSymbol decl
                       <> "' (address-of) not found in any loaded library")
                      msgT)
              Just fp -> pure (VPrimObj (PrimPtr (castPtr (castFunPtrToPtr fp))))
    | otherwise = collect []
  where
    argc = length (fdArgTypes decl)

    collect :: [Val] -> IO Val
    collect acc
        | length acc >= argc = do
              -- All args collected — dispatch (or build IO action).
              let call = callForeign decl (reverse acc)
              if fdIsIO decl then pure (VIO call) else call
        | otherwise = pure $ VFun $ \t -> do
              v <- force legacyHooks t
              collect (v : acc)
