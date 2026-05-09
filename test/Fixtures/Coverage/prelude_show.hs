-- @show@ source-loaded from @GHC.Internal.Show@.
--
-- The bare-name @show@ shim was dropped from @IHC.Builtins.builtins@
-- per CLAUDE.md's "Builtin modules: minimum surface only" rule.
-- Resolution now flows through the source-loaded @class Show@ in
-- @ghc-internal/GHC/Internal/Show.hs@:
--
--   * 'IHC.Scheduler.resolveBarePrelude' lazy-loads
--     @GHC.Internal.Show@ on first @show@ reference (it's in
--     @preludeScope@).
--   * 'registerGlobalLoadedModule' merges the loaded module's
--     @scanClassDecls@ output into 'globalMethodClassRef', so the
--     env-fallback's 'tryClassMethodFromRegistry' synthesises a
--     'classMethodDispatcher' for @show@ on demand.
--   * The dispatcher tries the @Show T.show@ instance first.  When
--     the instance is missing or registered as 'methodPlaceholder'
--     (e.g. @instance Show Int.showsPrec = showSignedInt@, whose body
--     uses primop unboxing patterns @(I# n)@ the parser doesn't yet
--     handle), 'IHC.Scheduler.hostShowFallback' delegates to
--     'IHC.Builtins.showValWith' — exactly the same workhorse
--     formatter the old shim used.
--   * User @instance Show T@ bodies always win over the host
--     fallback because the dispatcher consults the registry before
--     'hostShowFallback' fires.
--
-- Lock down the cases the bare-name shim used to handle:
--
--   * primitive Int (positive, negative, zero),
--   * Char (printable + escape),
--   * Bool,
--   * unit (),
--   * Double formatting (whole + fractional),
--   * String (round-trip with quotes),
--   * list of primitives,
--   * tuple,
--   * Ordering (deriving Show on the prelude type),
--   * user @instance Show@ with a recursive @show@ call inside the
--     instance body (the recursive call must also resolve through
--     the dispatcher path, not the now-removed bare-name shim),
--   * @deriving Show@ on a user sum type,
--   * @print@ — sanity check that the source-loaded
--     @print x = putStrLn (show x)@ still works once @show@ became
--     a dispatcher.
data Shape = Circle Int | Rectangle Int Int
data Color = Red | Green | Blue deriving (Show)

instance Show Shape where
    show (Circle r)      = "Circle(r=" ++ show r ++ ")"
    show (Rectangle w h) = "Rect(" ++ show w ++ "x" ++ show h ++ ")"

main :: IO ()
main = do
    -- Primitive Int.
    putStrLn (show (42 :: Int))
    putStrLn (show (-17 :: Int))
    putStrLn (show (0 :: Int))
    -- Char.
    putStrLn (show 'x')
    putStrLn (show '\n')
    -- Bool.
    putStrLn (show True)
    putStrLn (show False)
    -- Unit.
    putStrLn (show ())
    -- Double — whole and fractional.
    putStrLn (show (3.14 :: Double))
    putStrLn (show (1.0  :: Double))
    -- String round-trip.
    putStrLn (show "hello")
    -- List of primitives.
    putStrLn (show [1, 2, 3 :: Int])
    -- Tuple.
    putStrLn (show (1 :: Int, 'a', True))
    -- Ordering — deriving Show in GHC.Internal.Show source.
    putStrLn (show LT)
    putStrLn (show EQ)
    putStrLn (show GT)
    -- User instance with a recursive show call inside the body.
    putStrLn (show (Circle 5))
    putStrLn (show (Rectangle 3 4))
    -- Derived Show on a user sum type.
    putStrLn (show Red)
    putStrLn (show Green)
    putStrLn (show Blue)
    -- print — slice-4 source body @print x = putStrLn (show x)@.
    print (99 :: Int)
    print (Circle 7)
