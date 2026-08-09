{-# LANGUAGE ImplicitParams #-}
module Main where

import GHC.Internal.Exception
import GHC.Internal.Exception.Context (ExceptionContext(..))

main :: IO ()
main = do
    se <- toExceptionWithBacktrace (ErrorCall "source backtrace")
    case someExceptionContext se of
        ExceptionContext []    -> putStrLn "missing backtrace context"
        ExceptionContext (_:_) -> putStrLn "source backtrace context"
