# Interactive Haskell Computer (`ihc`)

A demand-driven, single-pass, copy-and-patch JIT interpreter for Haskell targeting **macOS / Apple Silicon only**.

Goals (see `CLAUDE.md`):

- Fast, multi-core interpreter capable of running real Hackage applications — ultimately **IHP** and **Warp** servers.
- **Demand-driven**: only parse/rename/run bindings transitively reachable from `main`. Unused top-level bindings are never parsed.
- **Minimal layers**: Pascal-style — source streams straight into emitted aarch64 machine code with no AST or Core IR in between.
- **Hot loop in aarch64**: stencils compiled by GHC/Clang, patched and stitched at runtime.
- **Optimistic typing**: the background type checker runs *while* `main` is already executing; most programs never wait on it.

## Status

**Phase 0** — scaffolding. The JIT smoke test emits and executes aarch64 in a `MAP_JIT` page. Phase 1 (the demand-driven single-pass JIT for pure Int programs) is next.

## Dev setup

Requires macOS on Apple Silicon and Nix with flakes enabled.

```sh
direnv allow            # or: nix develop
make test               # build + codesign + run the JIT smoke tests
make check              # build + codesign + run `ihc --check-jit`
```

## Why codesigning is necessary

Apple Silicon's hardened runtime forbids `PROT_EXEC` on writable pages unless the binary carries `com.apple.security.cs.allow-jit`. `scripts/codesign-jit.sh` ad-hoc-signs the binary with the required entitlements. The `Makefile` runs it automatically. On CI (`macos-14` runner) the same script is invoked.

## Layout

| Path | Purpose |
|---|---|
| `rts/jit.c`, `rts/jit.h` | `MAP_JIT` allocator + `pthread_jit_write_protect_np` W↔X toggle + `sys_icache_invalidate` |
| `rts/bridge.c` | (Phase 1+) glue for GHC RTS allocation and `dlopen`/`dlsym` |
| `rts/enter.S` | (Phase 1+) STG-style enter/apply in aarch64 asm |
| `src/IHC/Jit.hs` | thin FFI binding around the C jit API |
| `app/Main.hs` | CLI |
| `test/JitSmoke.hs` | end-to-end JIT page proof |
| `jit.entitlements` | `allow-jit`, `allow-unsigned-executable-memory`, `disable-library-validation` |
| `scripts/codesign-jit.sh` | ad-hoc codesigner |

See `/Users/marc/.claude/plans/temporal-mixing-raccoon.md` for the full bootstrap plan.
