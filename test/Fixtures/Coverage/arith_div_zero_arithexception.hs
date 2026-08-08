-- Regression: integer @div@ by zero must raise the real
-- @DivideByZero :: ArithException@ (base's @divZeroError = raise#
-- divZeroException@, with @divZeroException@ re-exported from
-- GHC.Internal.Exception.Type through GHC.Internal.Exception).  The
-- export-subs parser bug (see export_dotdot_bundled_patsyn) had zeroed
-- GHC.Internal.Exception's import list, so @divZeroException@ was
-- unresolvable and @div 1 0@ died with "unbound variable
-- GHC.Internal.Exception.divZeroException" instead.  We match the
-- @DivideByZero@ constructor directly — an interpreter-internal error
-- value would fall through to the @other@ arm.
import Control.Exception

main :: IO ()
main = do
    r <- try (evaluate (div (1 :: Int) 0)) :: IO (Either ArithException Int)
    case r of
        Left DivideByZero -> putStrLn "divide by zero"
        Left _            -> putStrLn "other arith exception"
        Right v           -> print v
