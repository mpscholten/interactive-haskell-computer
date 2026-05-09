-- Cross-binding references that force the env-fallback hook to fire
-- for bare names @a@ and @b@ AFTER Prelude / GHC.Internal.Base have
-- been hauled in by the @+@ class-method dispatch.  The combination
-- exercised here was the trigger for the Linux-CI heap-exhaustion
-- (4 GB / >7 M discoverInModule calls) that the negCache "mark done
-- on every walk" change in 'IHC.Scheduler.discoverInModuleWith'
-- closed off — the qualified-path of 'discoverInModuleWith'' used to
-- re-enter @discoverInModule GHC.Internal.Base "a"@ on every retry
-- because the parser-reported (but-never-real) binding @a@ from
-- @ghc-internal/.../Base.hs@'s @a `shiftL#` b = ...@ infix-binding
-- lines never landed in the negCache.
--
-- Lock down the patterns so a future regression in the discovery
-- cache (or in the env-fallback hook) trips this fixture before it
-- trips the @discoveryCallCap@ safety net at the suite level.

main :: IO ()
main = do
    print (a + b)        -- 42  — bare cross-binding
    print (a * b)        -- 320 — same pair through *
    print (negate a)     -- -10 — exercises ENeg → negate dispatch
    print (a + b + 1)    -- 43  — chained + dispatch

a :: Int
a = 10

b :: Int
b = 32
