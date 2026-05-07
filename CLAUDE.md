# Interactive Haskell Computer

We are building a very efficient super fast haskell interpreter.

It should be able to run any haskell application on hackage. Specifically IHP and warp-based server apps.

It should effienctly use all cores given to the intrepter.
In interpreter mode it should delay type checking as far as possible.
It should be optimistic and should rely on the property that most haskell code is well typed most of the time.


One problem of GHC is that it does to much work all the time.

E.g. lets say we have a program:
```haskell
main = putStrLn "hello"

a = ...
b = ...
c = ..
```

Our intrepter should only parse, rename, and run `main` bindings, but ignore a, b, c (maybe type check im the background once main is runnoing)


Our compiler target is macOS only. Our interpreter loop should be a highly efficient aarch64 asm code (maybe written as direct assembly instead of haskell).

Ideally we run the haskell without much intermrediate layers. We should be inspiured of the old very fast pascal compilers.

## Builtin modules: minimum surface only

Keep the set of host-backed / "builtin" modules to an absolute minimum. The only modules that may be host-backed are those with **no Haskell source at all** — the compiler-built modules like `GHC.Prim`, `GHC.Types`, `GHC.Magic`. Anything with real `.hs` source in `base` (or in any Hackage package) must be interpreted from that source, not short-circuited via a builtin shim.

### Rules for adding a new builtin or builtin-backed module

Before adding anything to `isBuiltinBackedModule` or the primop catalog in `IHC.Builtins`, ALL of the following must hold:

1. **No source.** The module/symbol must have no `.hs` source in the relevant package (check `~/.cache/ihc/sources/<pkg>-<version>/`). If source exists, interpret it.
2. **Compiler-intrinsic OR RTS-exclusive.** The symbol is either a GHC primop (`GHC.Prim`), a compiler-built type (`GHC.Types`), or something that inherently can only live in the host RTS (e.g. a `ForeignPtr` allocation — no userland Haskell code could implement the underlying allocator).
3. **Documented justification.** Every whitelist entry in `isBuiltinBackedModule` must carry an inline comment explaining *why* it can't be source-loaded. "It's more convenient" is not a valid reason.
4. **No shims for ordinary Hackage libraries.** Do NOT host-shim `tasty`, `optparse-applicative`, `containers`, `Data.Text`, `aeson`, etc. — even if it's faster to ship. Those are ordinary Haskell; we interpret them. (See the Phase 2.10b abandonment and the tasty/optparse shim removal for precedent.)

If interpreting a module from source reveals a missing language extension, primop, or class-dispatch case, the correct response is to **implement the missing feature**, not to add another shim. This keeps the interpreter honest and exercises the full parser/evaluator path.

### Tracked carve-outs (known violations, blocked on separate work)

- **`Data.ByteString` shims** at `src/IHC/Builtins.hs:317-375` — source-loading `Data.ByteString` works correctly but takes ~9 minutes to complete because Scheduler discovery cascades through `GHC.Internal.Show`'s transitive closure (see the inline comment at `Builtins.hs:3155-3161`). Tolerated until Scheduler discovery latency is bounded; then the shims graduate by deletion.
- **VIO ↔ State# bridges** at `src/IHC/Builtins.hs:1089-1140` (`unIO`, `ioToST`/`unsafeIOToST`, `stToIO`/`unsafeSTToIO`) — truly RTS-exclusive. The VIO runtime representation cannot be expressed as a source-level `State# RealWorld -> (# State# RealWorld, a #)` function; these bridges are compiler-intrinsic in the same way `unsafeCoerce` is (see the `Unsafe.Coerce` justification at `Scheduler.hs:5493-5500`). Documented inline; not for removal.

## Fixing interpreter bugs: reproduce → fixture → fix → verify

When real Hackage code (warp, IHP, blaze-html, …) fails with an interpreter error, **do not start by editing `src/IHC/`**. Follow this loop — it has a much higher hit rate and ships a regression test with every fix:

1. **Reduce.** Boil the failure down to the smallest standalone `.hs` program that hits the same error signature (same constructor list in the `PatternMatchFail` message, same dispatcher error, etc.). Use a custom ADT so the fixture doesn't depend on having the failing package interpretable end-to-end.
2. **Fixture.** Drop the reduced program into `test/Fixtures/Coverage/<name>.hs` with a matching `<name>.out` golden-stdout file. The Coverage suite auto-discovers fixtures — no test wiring needed. See the module header in `test/Unsupported.hs` for the Coverage / Unsupported split and graduation rules.
3. **RED.** Run `direnv exec . cabal test ihc-test` and confirm the fixture fails with the same error signature as the original. If it doesn't, the reduction lost something — refine.
4. **Fix at the interpreter level.** Parser, scheduler, or evaluator — wherever the bug lives. Do **not** paper over it by adding a host-shim or builtin (the "Builtin modules: minimum surface only" rule above still applies). Add a one-line stderr trace if needed to localise the failure; revert the trace before committing.
5. **GREEN + baseline.** Re-run `cabal test ihc-test`. Confirm both (a) the new fixture passes, and (b) the existing baseline failure count is unchanged or smaller. A new failure elsewhere means the fix over-corrected and regressed something else.
6. **Commit fixture + fix together.** One commit, both files. The fixture is now a permanent canary against future regressions.

Worked example: commit `5b1d33c` (`Parser: parseBindingsIn seeds cursor with source line/col, not (1,1)`). One source file (`src/IHC/Parser.hs`) + two fixtures (`test/Fixtures/Coverage/where_multiclause_function.hs`, `where_multiclause_3args.hs`). Tests went 992 → 994 with no change to existing pass/fail counts.

When **not** to use this loop: for language features we genuinely haven't implemented yet (a missing extension, an unknown primop, etc.), the fixture goes under `test/Fixtures/Unsupported/` with a `-- Gap:` comment. The Unsupported suite emits those as `pendingWith`, so they document the gap without breaking CI. Once the feature lands, move the fixture to `test/Fixtures/Coverage/`.