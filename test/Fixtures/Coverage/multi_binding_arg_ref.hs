-- Regression: a top-level binding referenced from the argument position
-- of a non-control-flow application must be resolvable.
--
-- 'discoveryFreeVars' deliberately does NOT chase function arguments
-- outside strict-control ops (bracket/finally/onException/...). Without
-- a separate fallback path, programs as small as `foo = 42; main =
-- print foo` fail with `IHC.Eval: unbound variable foo`. The
-- 'tryEntryModuleBinding' branch in 'resolveBarePrelude' catches this
-- case before the bare-name fallback gives up to Prelude.

foo = 42

main = print foo
