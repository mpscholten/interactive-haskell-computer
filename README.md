# Interactive Haskell Computer (`ihc`)

A from-scratch Haskell interpreter targeting **macOS / Apple Silicon only**. Goal: interpret real Hackage source — eventually the bytestring test suite, then Warp/IHP.

## Status (Phase 2.7 — 2026-04-15)

- **94/94 tests pass** (84 fixtures + 10 Cabal-loader tests) through a tree-walking lazy evaluator.
- **`Data/ByteString/Lazy.hs` + the rest of bytestring (55/55 files = 100%) parse cleanly.**
- **Cabal-aware source loader**: detects project root, reads `cabal.project.freeze`, parallel `cabal get` into `~/.cache/ihc/sources/`, per-package extensions/cpp-options.
- Lazy evaluation with `IORef`-backed thunks (black-hole protocol).
- ADTs + pattern matching + lists + `[Char]` strings + tuples + as/bang patterns.
- Multi-clause functions + guards.
- Pratt operator parser with module-level fixity tables.
- Hand-rolled CPP (no `cpphs` dep) handling `#if`/`#ifdef`/`MIN_VERSION_*`/etc.
- IO monad with `do`/`<-`/`>>=`/`return`/IORef/file IO/exit.
- Multi-module loading with qualified imports + per-module `KnownSymbols`.
- Lambdas (multi-arg + `\case`), sections, backtick infix, `$`, `.`, `MultiWayIf`.

Everything via interpretation — **no JIT path on the runtime today**. The aarch64 JIT from Phase 1 is dormant in `src/IHC/{Jit,Encode,Emit,IR,CodeBuffer,Stdlib}.hs` and `rts/`.

## Roadmap

The north-star: **run bytestring's `tests/Main.hs` from source under `ihc`** with the same pass/fail count as `cabal test`. Phase plan in `/Users/marc/.claude/plans/temporal-mixing-raccoon.md`.

Remaining slices (rough order):
- 2.3 — type classes (dictionary passing) — designed, in progress
- 2.7 — Cabal-aware source loader — ✅ shipped
- 2.12 — tasty/QuickCheck end-to-end pipeline — designed
- 2.8 — `ByteArray#`/`ForeignPtr`/`Word8`/`Storable` primops — designed
- 2.9 — mid-milestone: `L.putStr (L.pack [72,105,10])` runs from source
- 2.9.5 — GADTs + `Typeable`/`cast`/`Dynamic` — surfaced by tasty survey
- 2.10a — STM + async exceptions, 2.10b — trusted host modules (containers shimmed)
- 2.11 — `Lift`-splice TH (subset of full TH)
- 2.12 — tasty-load pipeline integration
- 2.13 — ⭐ bytestring test suite

## Dev setup

```sh
direnv allow                # or: nix develop
nix develop -c cabal build all
nix develop -c cabal test ihc-test --test-show-details=streaming
nix develop -c cabal run ihc -- run path/to/program.hs
```

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
| `src/IHC/Val.hs` | `Val`, `Thunk`, `Env`, `PrimObj` |
| `src/IHC/Eval.hs` | force / eval / apply / matchPat |
| `src/IHC/Builtins.hs` | host-Haskell primitives (no FFI shims) |
| `src/IHC/Scheduler.hs` | discovery + multi-module loading + tying-the-knot |
| `src/IHC/Driver.hs` | CLI entry point: file → search-path → eval → exit code |
| `src/IHC/Jit.hs` + `src/IHC/{Encode,Emit,IR,CodeBuffer,Stdlib}.hs` + `rts/*` | dormant Phase-1 aarch64 JIT (no longer on runtime path) |
| `test/RunFile.hs` | golden-output fixture tests |
| `test/Fixtures/` | `.hs` programs the suite runs |

See `CLAUDE.md` and `/Users/marc/.claude/plans/` for the full design history (one plan per phase).
