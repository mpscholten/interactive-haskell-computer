-- Locks down the source-loaded standalone @deriving instance Eq T@
-- synthesis added by 'IHC.Scheduler.registerOneStandaloneEq' for the
-- types that @GHC.Classes@ derives Eq for via separate
-- @deriving instance@ declarations (rather than an in-line @data ...
-- deriving Eq@):
--
--   * @data Bool = False | True@   in @GHC.Types@
--     @deriving instance Eq Bool@  in @GHC.Classes@
--   * @data Ordering = LT|EQ|GT@   in @GHC.Types@
--     @deriving instance Eq Ordering@ in @GHC.Classes@
--
-- The standalone-deriving scanner ('IHC.Scan.scanStandaloneDerivings')
-- picks up the @deriving instance@ declarations; the registrar
-- cross-references the type name against the union of every loaded
-- module's 'lmTypeCtorReg' to find @T@'s constructors.
--
-- Polymorphic @eqIt@ forces the elaborator to emit
-- @ETypedMethod \"Eq\" \"==\" tag@, so the synthesised structural body
-- fires through the same class-dispatch path used by bare @==@.
eqIt :: Eq a => a -> a -> Bool
eqIt x y = x == y
{-# NOINLINE eqIt #-}

main :: IO ()
main = do
    print (eqIt True  True)        -- True
    print (eqIt True  False)       -- False
    print (eqIt False False)       -- True
    print (eqIt LT LT)             -- True
    print (eqIt LT EQ)             -- False
    print (eqIt GT GT)             -- True
