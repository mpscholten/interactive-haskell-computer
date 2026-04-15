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
import Foreign.C.String (CString, newCAString, peekCString)
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

ihcMod :: Int -> Int -> Int
ihcMod a b = a `mod` b

ihcDiv :: Int -> Int -> Int
ihcDiv a b = a `div` b

ihcAbs :: Int -> Int
ihcAbs = abs

ihcMin :: Int -> Int -> Int
ihcMin = min

ihcMax :: Int -> Int -> Int
ihcMax = max

-- | @show :: Int -> String@ — monomorphised to Int. Returns a fresh
-- C-allocated NUL-terminated string. The host process exits shortly
-- after a JIT run, so leaking is acceptable.
ihcShowInt :: Int -> IO CString
ihcShowInt n = newCAString (show n)

-- | @(++) :: String -> String -> String@ on C strings. Allocates a
-- new buffer holding the concatenation.
ihcConcat :: CString -> CString -> IO CString
ihcConcat a b = do
    sa <- peekCString a
    sb <- peekCString b
    newCAString (sa ++ sb)

-- | @getLine :: IO String@ — read a line from stdin, return as fresh
-- C-allocated string (without the trailing newline).
ihcGetLine :: IO CString
ihcGetLine = getLine >>= newCAString

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

foreign export ccall "ihc_mod" ihcMod :: Int -> Int -> Int
foreign import ccall unsafe "&ihc_mod"
    p_ihcMod :: FunPtr (Int -> Int -> Int)

foreign export ccall "ihc_div" ihcDiv :: Int -> Int -> Int
foreign import ccall unsafe "&ihc_div"
    p_ihcDiv :: FunPtr (Int -> Int -> Int)

foreign export ccall "ihc_abs" ihcAbs :: Int -> Int
foreign import ccall unsafe "&ihc_abs"
    p_ihcAbs :: FunPtr (Int -> Int)

foreign export ccall "ihc_min" ihcMin :: Int -> Int -> Int
foreign import ccall unsafe "&ihc_min"
    p_ihcMin :: FunPtr (Int -> Int -> Int)

foreign export ccall "ihc_max" ihcMax :: Int -> Int -> Int
foreign import ccall unsafe "&ihc_max"
    p_ihcMax :: FunPtr (Int -> Int -> Int)

foreign export ccall "ihc_showInt" ihcShowInt :: Int -> IO CString
foreign import ccall unsafe "&ihc_showInt"
    p_ihcShowInt :: FunPtr (Int -> IO CString)

foreign export ccall "ihc_concat" ihcConcat :: CString -> CString -> IO CString
foreign import ccall unsafe "&ihc_concat"
    p_ihcConcat :: FunPtr (CString -> CString -> IO CString)

foreign export ccall "ihc_getLine" ihcGetLine :: IO CString
foreign import ccall unsafe "&ihc_getLine"
    p_ihcGetLine :: FunPtr (IO CString)

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
    [ ("putStrLn", Builtin (castFunPtrToPtr p_ihcPutStrLn) 1)
    , ("print",    Builtin (castFunPtrToPtr p_ihcPrintInt) 1)
    , ("putChar",  Builtin (castFunPtrToPtr p_ihcPutChar)  1)
    , ("mod",      Builtin (castFunPtrToPtr p_ihcMod)      2)
    , ("div",      Builtin (castFunPtrToPtr p_ihcDiv)      2)
    , ("abs",      Builtin (castFunPtrToPtr p_ihcAbs)      1)
    , ("min",      Builtin (castFunPtrToPtr p_ihcMin)      2)
    , ("max",      Builtin (castFunPtrToPtr p_ihcMax)      2)
    , ("show",     Builtin (castFunPtrToPtr p_ihcShowInt)  1)
    , ("getLine",  Builtin (castFunPtrToPtr p_ihcGetLine)  0)
      -- (++) is bound at the operator level via a synthetic ICall
      -- with this name; see IHC.Parser.parseSum.
    , ("##concat", Builtin (castFunPtrToPtr p_ihcConcat)   2)
    ]
