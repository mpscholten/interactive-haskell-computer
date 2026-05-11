-- Regression: 'fork#' on an UNWRAPPED IO action (the @\\s -> ...@
-- function extracted from @IO io'@ pattern match) must execute the
-- action body in the new thread, not just return without running.
--
-- This is exactly warp's @defaultFork@ shape:
--
--   defaultFork io =
--       IO $ \\s0 ->
--           case io unsafeUnmask of
--               IO io' ->
--                   case fork# io' s0 of
--                       (# s1, _tid #) -> (# s1, () #)
--
-- where @io'@ is the State#-passing function from the @IO io'@
-- pattern match.  Before this fix, IHC's host @forkHashB@ called
-- @runIOVal av@ on the function, but @runIOVal@'s fallthrough is
-- @pure v@ for non-VIO/non-VCon-IO values — so the action body was
-- skipped and the connection handler thread silently exited.
--
-- The user-visible symptom of this regression was warp_hello accepting
-- the TCP connection but never sending a response (curl exited 56 /
-- connection-reset).
import Control.Concurrent (MVar, newEmptyMVar, takeMVar, putMVar)
import GHC.IO (IO (..))
import GHC.Conc.Sync (forkIO)
import GHC.Prim (fork#)
import GHC.Types (IO (..))

-- Mirror warp's defaultFork: takes a (forall a. IO a -> IO a) action,
-- unwraps the IO via 'IO io'' pattern match, calls fork# on the
-- raw State#-passing function.  This is the EXACT shape that
-- exercises forkHashB on a VFun (the unwrapped State# function).
defaultForkLike :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
defaultForkLike io =
    IO $ \s0 ->
        case io id of
            IO io' ->
                case fork# io' s0 of
                    (# s1, _tid #) -> (# s1, () #)

main :: IO ()
main = do
    mvar <- newEmptyMVar
    defaultForkLike $ \_unmask -> do
        putMVar mvar "child action executed!"
    msg <- takeMVar mvar
    putStrLn msg
