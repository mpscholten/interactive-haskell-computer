{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Thin Haskell bindings around the C API in @rts/jit.h@.
--
-- Typical use:
--
-- > withJitPage 4096 $ \page size -> do
-- >     jitWritable
-- >     pokeMachineCode page bytes
-- >     jitExecutable
-- >     jitFlush page size
-- >     callAsInt64 page
module IHC.Jit
    ( -- * Allocation
      jitAlloc
    , jitFree
    , withJitPage
    , jitPageSize
      -- * W^X toggle
    , jitWritable
    , jitExecutable
      -- * Cache coherence
    , jitFlush
      -- * Codesign sanity check
    , codesignCheck
    ) where

import Control.Exception (bracket)
import Data.Word (Word64)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CInt(..), CSize(..))

foreign import ccall unsafe "jit.h ihc_jit_alloc"
    c_ihc_jit_alloc :: CSize -> IO (Ptr ())

foreign import ccall unsafe "jit.h ihc_jit_free"
    c_ihc_jit_free :: Ptr () -> CSize -> IO ()

foreign import ccall unsafe "jit.h ihc_jit_writable"
    c_ihc_jit_writable :: IO ()

foreign import ccall unsafe "jit.h ihc_jit_executable"
    c_ihc_jit_executable :: IO ()

foreign import ccall unsafe "jit.h ihc_jit_flush"
    c_ihc_jit_flush :: Ptr () -> CSize -> IO ()

foreign import ccall unsafe "jit.h ihc_jit_page_size"
    c_ihc_jit_page_size :: IO CSize

foreign import ccall unsafe "jit.h ihc_jit_codesign_check"
    c_ihc_jit_codesign_check :: IO CInt

jitAlloc :: Int -> IO (Ptr ())
jitAlloc n = do
    p <- c_ihc_jit_alloc (fromIntegral n)
    if p == nullPtr
        then error "IHC.Jit.jitAlloc: mmap(MAP_JIT) failed — check entitlements & hardened runtime"
        else pure p

jitFree :: Ptr () -> Int -> IO ()
jitFree p n = c_ihc_jit_free p (fromIntegral n)

-- | Allocate a JIT page, run an action with it, free afterwards.
-- Size is rounded up to the system page size internally.
withJitPage :: Int -> (Ptr () -> Int -> IO a) -> IO a
withJitPage n action = bracket (jitAlloc n) (\p -> jitFree p n) (\p -> action p n)

jitPageSize :: IO Int
jitPageSize = fromIntegral <$> c_ihc_jit_page_size

jitWritable, jitExecutable :: IO ()
jitWritable   = c_ihc_jit_writable
jitExecutable = c_ihc_jit_executable

jitFlush :: Ptr () -> Int -> IO ()
jitFlush p n = c_ihc_jit_flush p (fromIntegral n)

-- | Returns 0 on success, errno (or -1) on failure.
codesignCheck :: IO Int
codesignCheck = fromIntegral <$> c_ihc_jit_codesign_check
