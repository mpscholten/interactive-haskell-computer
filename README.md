# Interactive Haskell Computer (`ihc`)

A from-scratch Haskell interpreter targeting **macOS / Apple Silicon only**. Goal: interpret real Hackage source — eventually the bytestring test suite, then Warp/IHP.

## Agentic engineering

`ihc` is also an experiment in *agentic software engineering*: the bulk of the code is written by AI coding agents, one phase plan at a time. The harnesses driving this project so far are **Claude Code**, **Codex**, and **Hermes** — different agents, same plan-and-fixture workflow, different strengths on different slices. The interpreter's architecture is deliberately shaped to make that workflow productive — and conversely, the things that make `ihc` fast also turn out to be the things that make agents effective on it.

- **Sliceable phases.** Each language feature is a bounded phase with a fixture or test as the acceptance gate — type classes, ADT pattern match, do-notation, `ByteArray#`/`ForeignPtr` primops, STM, the warp listener path, and so on. One agent session, one slice, one merge. The numbered roadmap below is also the agent task queue.
- **No shims, ever.** Ordinary Hackage source is interpreted, never host-replaced. The only host-backed modules are compiler-built (`GHC.Prim`, `GHC.Types`, `GHC.Magic`) — see [`CLAUDE.md`](CLAUDE.md). Every missing piece surfaces as a concrete agent task — implement the primop, add the extension, fix dispatch — instead of being papered over with a fake. This keeps the interpreter honest and forces the long tail of the language to actually get built.
- **Demand-driven everything.** Parsing, name resolution, and evaluation walk only the closure of `main`. An agent touching one module doesn't pay for the whole codebase; the compile-error surface stays local to whatever's being changed. The same property is what lets `ihc` skip type-checking unused bindings at runtime.
- **Plan-driven.** Every non-trivial change starts with a written plan — problem, files, build sequence, verification — so the design intent stays inspectable across sessions and across different agents picking up adjacent slices.
- **Failure mode is "find the next slice".** When something doesn't run, the answer is rarely "patch the host" — it's "what tiny missing piece does the source need next?" That recasts every bug as a small, testable, agent-shaped task.

The runtime goal (Pascal-fast, source-on-demand, type-checking deferred) turns out to align cleanly with what agents do well: small steps, narrow blast radius, fixture-driven feedback. The project is as much a study of that alignment as it is a Haskell interpreter.

## Status (2026-08-08)

- **~495 coverage fixtures** exercise the tree-walking lazy evaluator (up from 421 on 2026-08-02, 332 in May).
- **Parser probe at 100% on the Hackage/IHP sample set.** `ihc-parse-probe` reports **zero parse errors** on top-level value-binding RHSs for: the representative IHP 50-file sample (**462/462** bindings OK), hasql, hasql-pool, servant, servant-server, lens `Control.Lens` (35-file core), and conduit. April baselines were ~79% (IHP) / ~69–94% (Hackage); residual gaps (dot-ops like `.=`/`.|`, char lits `'['`/`'{'`, comma/pattern guards, layout-`let` edges, TH name quotes / `$var` splices, `\\` as an operator) are closed. Re-run with `scripts/ihp-parse-probe.sh` / `scripts/hackage-parse-probe.sh` (or `exe:ihc-parse-probe`). Older gap notes in `ihp-parser-gaps.md` / `hackage-parser-gaps.md` are historical.
- **The great de-shimming.** Since May, ~110 host shims have been deleted and replaced with interpreted source. The `isBuiltinBackedModule` whitelist is now genuinely minimal — only source-less, compiler-intrinsic `GHC.*` modules (`GHC.Prim`, `GHC.Types`, `GHC.Prim.PrimOpWrappers`, `Unsafe.Coerce`, the TH splice stubs). Everything with real `.hs` source is now interpreted from that source, including:
  - **`Network.Socket`** — `socket`, `bind`, `listen`, `accept`, `connect`, `getAddrInfo`, `getSocketName`, buffer helpers, facade re-exports. No host socket shims remain.
  - **`Foreign` layer** — `Storable` (`peek`/`poke`/`sizeOf`/`alignment`/`peekElemOff`), `ForeignPtr` (`newForeignPtr`/`withForeignPtr`), `Foreign.Marshal.Alloc` (`malloc`/`free`), `CAString` byte helpers.
  - **Concurrency** — `forkIO`/thread/`throw`/`MVar`/`IORef` wrappers, `labelThread`, `Unique` generators, `System.TimeManager`, `GHC.Event` timers.
  - **`Num`/`Ord`/`RealFloat`/`Semigroup` methods**, `fromIntegral`, monad `>>=`/`>>`, `pure`/`return` — all source-loaded, dispatched through the generic class-method path.
  - **Full `ghc-bignum` `BigNat` pipeline** — word/array readers, comparisons, size predicates, index, `log`/`toWord`/`toInt`, square. All source-loaded; the shims are gone.
- **STM reworked onto primops.** `atomically`/`retry`/`orElse`/`catchSTM`/`newTVar`/`readTVar`/`writeTVar` are source-loaded from `GHC.Conc.Sync` and bottom out on `atomically#`/`retry#`/`newTVar#`/… primops. STM collapses onto IO at the (single-threaded) eval level, mirroring the `ST s ≈ IO` bridge.
- **Warp reaches request processing.** The full socket path source-loads; warp binds its port, accepts a connection, and receives + processes an HTTP request end-to-end. The remaining blocker to serving the first *response* is a post-receive CPU busy-spin in the interpreted response path (see below).
- **HSX hello-world (Phase 2.14) foundation in progress** — caches primed, smoke fixture, `EQuasiQuote` parser wiring; route from `[hsx|…|]` → TH `Exp` → `renderHtml` mapped in `docs/HSX-PATH.md`. Binding-level parse of IHP/HSX sources is no longer the gate; the remaining work is TH `quoteExp` dispatch and evaluating the blaze/HSX pipeline.
- **Lexer/parser surface** — package-qualified imports, Pratt fixity (including 2-char `.ops` like `.=`/`.|`/`.#`), layout `let` (semicolon multi-bind, `where` on bindings, local infix multiclause), comma/pattern guards, char lits for `'['`/`'('`/`'{'`, TH name quotes / id-splices, QuasiQuotes, record update/dot, MultiWayIf, `\case`, sections, backticks.
- **ByteString fully source-loaded**; exception handling (`try`/`handle`/`bracket`/`finally`/`mask`) source-loaded from ghc-internal.
- **Cabal-aware source loader**: detects project root, reads `cabal.project.freeze`, parallel `cabal get` into `~/.cache/ihc/sources/`, per-package extensions/cpp-options.
- **Type classes via dictionary passing**: `ClassRegistry` maps `(ClassName, TypeTag)` to method lists; a generic class-method dispatcher resolves instances — no per-method host shims.
- Lazy evaluation with `IORef`-backed thunks (thread-aware black-hole protocol); ADTs + pattern matching + lists + `[Char]` strings + tuples + as/bang patterns; hand-rolled CPP; IO monad; multi-module loading with qualified imports.
- **Cold-start 140x–280x faster than ghci/runghc** (0.018s vs 2.5s for small programs).

Everything via interpretation — **no JIT path on the runtime today**.

## What's next

The socket-FFI chain that used to block all I/O is resolved — `Network.Socket` is source-loaded end to end, and warp now binds, accepts, and receives a request. The frontier is **serving the first HTTP response**:

| Blocker | Where | Impact |
|---------|-------|--------|
| **Post-receive CPU busy-spin** — after `recv` completes, one interpreter thread burns a full core and the request never gets a response (curl times out). Ruled out: black-hole yield/retry, STM `retry`/`orElse` loops. It's a genuine CPU loop in interpreted code somewhere in warp's response-build/serialize path, still unlocated. | interpreted `warp` response path | Warp receives + processes the request but never replies. |
| **STM `retry` is collapse-only** — STM combinators source-load onto the `*#` primops but run as IO on a single-threaded evaluator, so a real blocking `retry`/`orElse` wait has no transactional scheduler behind it. Latent; will bite genuine STM-`retry` waits. | `Builtins.hs` STM primops | Blocks correct blocking-STM semantics. |

Diagnostic recipe for the spin: sample the spinning thread or add an eval-counter probe in `IHC.Eval.force`; the loop is emergent from warp's full closure and hasn't reduced standalone.

**HSX hello-world (Phase 2.14)** is the other active milestone: interpret `[hsx|<h1>Hello world</h1>|]` → TH `Exp` → `renderHtml` end-to-end, no shims. Foundation phase is in progress (`docs/HSX-PATH.md`). Value-binding parse of the IHP/HSX sample is green; the open work is TH/QQ expansion and runtime evaluation of the blaze pipeline.

**Longer-term IHP readiness** is no longer gated on basic expression parsing for the packages above. Remaining work is mainly **semantic**: class dispatch at scale, full Template Haskell (`quoteExp`, quotation, `reify`), type families / DataKinds / OverloadedLabels / ImplicitParams elaboration, and the warp response-path spin. See the roadmap below and `ihp-unsupported-scan.md`.

## Roadmap

The north-star: **run bytestring's `tests/Main.hs` from source under `ihc`** with the same pass/fail count as `cabal test`. Each phase below is its own written plan (problem, files, build sequence, verification) — the plans live with the agent harness rather than in the repo.

Remaining slices (rough order):
- 2.3 — type classes (dictionary passing) — ✅ shipped
- 2.7 — Cabal-aware source loader — ✅ shipped
- 2.8 — `ByteArray#`/`ForeignPtr`/`Word8`/`Storable` primops + `GHC.Exts` surface — ✅ shipped (`Storable`/`ForeignPtr`/marshal all source-loaded)
- 2.9 — mid-milestone: `L.putStr (L.pack [72,105,10])` runs from source — ✅ shipped
- 2.9.5 — GADTs + `Typeable`/`cast`/`Dynamic` — surfaced by tasty survey
- 2.10a — STM + async exceptions — ✅ combinators source-loaded onto `*#` primops (blocking `retry` semantics still collapse-only; see What's next)
- 2.11 — `Lift`-splice TH (subset of full TH)
- 2.12 — tasty/QuickCheck/optparse-applicative source-load + end-to-end run (no shims; same rule as 2.10b) — designed
- ⚙️ **warp listener path** — socket layer source-loaded; binds/accepts/receives; blocked on the post-receive spin (see What's next)
- 2.13 — ⭐ bytestring test suite (north-star)
- 2.14 — **HSX hello-world milestone**: interpret `[hsx|<h1>Hello world</h1>|]` end-to-end (lex → `EQuasiQuote` → `IHP.HSX.QQ.hsx.quoteExp` → TH `Exp` → `IHC.AST.Expr` → `renderHtml`). Foundation phase in progress — caches populated, smoke fixture, parser wiring, docs in `docs/HSX-PATH.md`.
- 2.16 — **cold-start latency benchmark** vs `ghci :load` + `cabal run` on a real IHP project. Hypothesis: we win on first-request-served time because we skip type checking, Core pipeline, linking, and load sources on demand rather than eagerly.
- 3.1 — **full Template Haskell**: `[|…|]` quotation, `$(…)` AST-returning splices, `Q` IO, `reify`. Required by `aeson-th`, `lens` `makeLenses`, `persistent` TH — anything IHP uses heavily.
- 3.2 — **type families** (open, closed, associated). Required by `servant`, effect libs, `singletons`, some IHP generated code.
- 3.3 — **`DerivingVia` / `GeneralizedNewtypeDeriving` beyond trivial**, `QuantifiedConstraints`. Required by `optics`, `generic-lens`, modern typeclass-heavy Haskell.
- 3.4 — `DataKinds` + promoted types. Required by type-level routing (`servant`) and pervasive in IHP.
- 3.5 — `OverloadedLabels` (`#fieldName`). Pervasive in IHP queries — every `filterWhere` call uses it.
- 3.6 — `ImplicitParams` (`?context`, `?schema`). Every IHP controller relies on implicit context.

## Dev setup

```sh
direnv allow                # or: nix develop
nix develop -c cabal build all
nix develop -c cabal test ihc-test --test-show-details=streaming
nix develop -c cabal run ihc -- run path/to/program.hs
```

### Populating the source cache for test-framework packages

The nix flake's `ihcSourceRoot` ships sources for the common runtime
dependencies (hspec, QuickCheck, HUnit, aeson, attoparsec, …) but NOT the
`tasty` family, which the bytestring / aeson / attoparsec test suites import.
Fetch them once into the user-local cache:

```sh
scripts/cache-test-deps.sh
```

This populates `~/.cache/ihc/sources/tasty-*`, `tasty-quickcheck-*`,
`tasty-hunit-*`, `tasty-ant-xml-*`, and their transitive Hackage-only deps
(`ansi-terminal`, `ansi-terminal-types`, `colour`, `unbounded-delays`,
`generic-deriving`, `xml`).  Idempotent — safe to re-run.  Pass extra package
names as positional args to fetch additional tarballs.

## Layout

| Path | Purpose |
|---|---|
| `src/IHC/Source.hs` | immutable source buffer + cursors |
| `src/IHC/Lexer.hs` | streaming layout-aware lexer; pragmas, blocks, MagicHash |
| `src/IHC/Cpp.hs` | hand-rolled CPP preprocessor |
| `src/IHC/Scan.hs` | demand-driven binding finder + `data` / fixity / type-sig scanners |
| `src/IHC/ModuleHeader.hs` | `module Foo where`, `import qualified … as …` parsing |
| `src/IHC/AST.hs` | `Expr`, `Pat`, `Lit`, `Stmt` |
| `src/IHC/Parser.hs` | recursive-descent → AST, Pratt operator layer |
| `probe/ParseProbe.hs` | `ihc-parse-probe` — binding-RHS parse rates over package trees |
| `scripts/*-parse-probe.sh` | IHP / Hackage sample drivers for the parse probe |
| `src/IHC/Val.hs` | `Val`, `Thunk`, `Env`, `PrimObj` |
| `src/IHC/Eval.hs` | force / eval / apply / matchPat |
| `src/IHC/Builtins.hs` | host-Haskell primitives (no FFI shims) |
| `src/IHC/Scheduler.hs` | discovery + multi-module loading + tying-the-knot |
| `src/IHC/Driver.hs` | CLI entry point: file → search-path → eval → exit code |
| `test/RunFile.hs` | golden-output fixture tests |
| `test/Fixtures/` | `.hs` programs the suite runs |

## Contributing

PRs are welcome — especially AI-engineered ones. If you've got an agent harness running (Claude Code, Codex, Hermes, Aider, anything) and spare tokens to burn, **point them at this repo**. The project is built that way and is meant to keep growing that way.

For your agent:

1. **Read [`CLAUDE.md`](CLAUDE.md) first.** It captures project conventions and the load-bearing rule — *no shims for ordinary Hackage libraries*. Interpret real source; implement the missing primop or extension. Push back if your agent reaches for a host shim.
2. **Pick a slice.** A roadmap entry above, a failing fixture under `test/Fixtures/`, or whatever surfaces when you run `cabal test`. Smaller is better — one feature, one merge.
3. **Add a fixture or test as the acceptance gate.** Every shipped slice in this repo has one. It's both the spec for the change and the proof it works. PRs without a test are unlikely to land.
4. **Open the PR.** Note the harness used, keep the diff focused on the slice, and avoid drive-by refactors.

Runtime errors like *"Non-exhaustive patterns in ..."*, *"Unknown primop"*, or *"Class C not in scope"* are usually the next agent-sized task in disguise — pick one and follow the trail.

---

See [`CLAUDE.md`](CLAUDE.md) for project conventions and the no-shims rule that constrains agent work. Per-phase plans (one plan per shipped slice) live with the agent harness rather than in the repo.
