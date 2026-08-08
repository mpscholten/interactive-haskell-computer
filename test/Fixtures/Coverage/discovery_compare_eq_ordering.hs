-- REGRESSION CANARY for source-loaded @==@/@/=@ discovery.
--
-- This is the exact reproducer from IHC.Builtins's former "drop ==//=" TODO:
-- composing a source-loaded @compare@ result with @==@ on @Ordering@.
-- Before the cascade fix, source-loading @==@/@/=@ made the per-FV
-- chase from @registerInstancesFrom@ / @registerClassDefaults@ recurse
-- through @GHC.Classes@'s Eq/Ord surface + the
-- @GHC.Internal.*@ web until the 4 GB heap exhausted (discovery total
-- ~1000 on master → >2400 → OOM).
--
-- The fix routes that chase through
-- @IHC.Scheduler.discoverInModuleForChase@ (curated
-- 'perFVChaseShortCircuit').  If the cascade ever regresses, an
-- isolated run of this fixture re-OOMs / trips
-- 'DiscoveryCallCapExceeded' → hard test failure.
main :: IO ()
main = do
    print (compare 1 2 == LT)
    print (compare 2 1 == LT)
    print (compare (1 :: Int) 1 == EQ)
    print (compare 'a' 'b' == LT)
    print (compare "abc" "abc" == EQ)
    print (compare 1 2 /= GT)
