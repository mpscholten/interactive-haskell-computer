-- Regression: class-method dispatch on a value built from a
-- *source-loaded* ADT ctor (e.g. warp's @StdMethod = GET | POST | …@)
-- must key on the type name, not the ctor name.
--
-- Before the 'ctorTypeHookRef' install, 'IHC.Classes.typeTagOf'
-- on @VCon "GET" []@ returned @"GET"@.  When warp's
-- @methodArray ! m@ in 'Network.HTTP.Types.Method' dispatched the
-- @Ix@ class, the bounds tag came out as @"GET"@/@"PATCH"@ rather
-- than @"StdMethod"@; the host @Ix Int@ shim then took over (because
-- @Int@ happened to be a dispatchable tag) and surfaced
-- @"Ix Int.index: non-Int index"@ when it tried to subtract @lo@
-- from the StdMethod ctor.
--
-- The hook walks the global module catalogue's 'lmDataReg' to map
-- ctor names to type names, so 'typeTagOf (VCon "GET" _)' now
-- returns @"StdMethod"@ and the source-loaded @Ix StdMethod@
-- instance handles the dispatch.
data Color = Red | Green | Blue
    deriving (Show, Eq, Ord, Enum, Bounded)

main :: IO ()
main = do
    -- Eq dispatch: keys on type name.  Before the hook, this
    -- looked for @Eq Red@ and @Eq Green@ — distinct types in the
    -- dispatcher's view — and fell through to a host shim or
    -- structural equality that may or may not have done the right
    -- thing.  After: both VCon's typeTagOf returns "Color".
    print (Red == Red)
    print (Red == Green)
    -- Ord dispatch: same story, plus the comparison must respect
    -- decl order.
    print (compare Red Blue)
    print (compare Green Red)
    -- Enum: ensures fromEnum / toEnum dispatch is keyed on Color.
    print (fromEnum Blue)
