-- Coverage for GHC.Prim.getCurrentCCS#, the source-less cost-centre
-- primop that GHC.Internal.Stack.CCS.currentCallStack bottoms on:
--
--   currentCallStack = ccsToStrings =<< getCurrentCCS ()
--   getCurrentCCS dummy = IO $ \s ->
--     case getCurrentCCS# dummy s of (# s', addr #) -> (# s', Ptr addr #)
--
-- HasCallStack / error / megaparsec annotations reach this leaf via
-- errorCallWithCallStackException -> currentCallStack -> getCurrentCCS#.
-- Direct IHP.HSX.QQ.parseHsx hits the same unbound once parse/pretty
-- leftovers are out of the way.
--
-- IHC has no profiler CCS. The primop returns a null CCS, so
-- currentCallStack yields []. Must not die with unbound getCurrentCCS#.
import GHC.Stack (currentCallStack)

main :: IO ()
main = currentCallStack >>= print
