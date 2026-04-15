{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Host-side wrappers around selected functions from @base@, exposed
-- as C-callable symbols so JIT'd code can invoke them via @blr@.
--
-- The JIT calls these through the Apple AAPCS64 convention: arguments
-- in x0, x1, …; result in x0. Haskell's @Int@ corresponds to @int64_t@
-- on 64-bit macOS, so the Int-taking wrappers are already the right
-- shape. Strings come in as 'CString' (C NUL-terminated byte strings),
-- allocated by the scheduler's string pool and persistent until the
-- compile session ends.
--
-- The @foreign import ccall "&symbol"@ lines below let us take the
-- address of each export in Haskell (via a 'FunPtr'), which the
-- scheduler then pre-seeds into the address map that 'IHC.Emit' uses
-- to resolve 'ICall'. No 'dlopen' required — the symbols are inside
-- our own binary.
module IHC.Stdlib
    ( Builtin(..)
    , builtins
    ) where

import Data.ByteString (ByteString)
import Foreign.C.String (CString, peekCString)
import Foreign.Ptr (Ptr, FunPtr, castFunPtrToPtr)
import System.IO (hFlush, stdout)

--------------------------------------------------------------------------------
-- Host implementations
--------------------------------------------------------------------------------

ihcPutStrLn :: CString -> IO ()
ihcPutStrLn cs = do
    s <- peekCString cs
    putStrLn s
    hFlush stdout

ihcPrintInt :: Int -> IO ()
ihcPrintInt n = do
    print n
    hFlush stdout

ihcPutChar :: Int -> IO ()
ihcPutChar c = do
    putChar (toEnum (fromIntegral c))
    hFlush stdout

--------------------------------------------------------------------------------
-- Foreign exports + re-imports (to take their address)
--------------------------------------------------------------------------------

foreign export ccall "ihc_putStrLn" ihcPutStrLn :: CString -> IO ()
foreign import ccall unsafe "&ihc_putStrLn"
    p_ihcPutStrLn :: FunPtr (CString -> IO ())

foreign export ccall "ihc_printInt" ihcPrintInt :: Int -> IO ()
foreign import ccall unsafe "&ihc_printInt"
    p_ihcPrintInt :: FunPtr (Int -> IO ())

foreign export ccall "ihc_putChar" ihcPutChar :: Int -> IO ()
foreign import ccall unsafe "&ihc_putChar"
    p_ihcPutChar :: FunPtr (Int -> IO ())

--------------------------------------------------------------------------------
-- Registry: name -> (entry ptr, arity)
--------------------------------------------------------------------------------

data Builtin = Builtin
    { builtinAddr  :: !(Ptr ())
    , builtinArity :: !Int
    }

-- | The well-known source-level names this interpreter ships with.
-- 'IHC.Scheduler' pre-seeds these into the address map so any binding
-- body can call them without a parsed definition.
builtins :: [(ByteString, Builtin)]
builtins =
    [ ("putStrLn", Builtin (castFunPtrToPtr p_ihcPutStrLn)  1)
    , ("print",    Builtin (castFunPtrToPtr p_ihcPrintInt) 1)
    , ("putChar",  Builtin (castFunPtrToPtr p_ihcPutChar)  1)
    ]
