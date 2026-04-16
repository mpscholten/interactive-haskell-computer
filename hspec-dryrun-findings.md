# Hspec Source-Load Diagnostic Findings

**Date:** 2026-04-16
**IHC commit:** ca0594c (master HEAD)
**Hspec version probed:** hspec-2.11.16 (nix), hspec-core-2.11.16, hspec-expectations-0.8.4
**User cache:** hspec-2.11.17, hspec-core-2.11.17 (one patch version newer; same structure)

---

## Probe Matrix

| # | Program | Result |
|---|---------|--------|
| 1 | `import Test.Hspec; main = putStrLn "ok"` | **PASS** — import chain resolves |
| 2 | `import Test.Hspec.Expectations; main = putStrLn "ok"` | **PASS** |
| 3 | `import Test.Hspec.Expectations (shouldBe); main = (1::Int) \`shouldBe\` 1` | **FAIL** — `unbound variable 'shouldBe'` |
| 4 | `import Test.Hspec; main = hspec $ describe "x" $ it "y" $ ...` | **FAIL** — `unbound variable 'hspec'` |

---

## Blocker #1 (Critical): Library-module load guard silently drops all cache-package imports

**Severity:** Root cause of failures in probes 3 and 4.
**Error:** `unbound variable 'shouldBe'` / `unbound variable 'hspec'`

### Root cause

In `Scheduler.hs`, `resolveImport` (line ~1135-1146) has an OOM guard:

```haskell
shouldLoad <- case Map.lookup (impModule imp) reg of
    Just _  -> pure True   -- already loaded, safe to query
    Nothing -> isLocalModule searchPath (impModule imp)
if not shouldLoad
    then tryImports rest
```

`isLocalModule` returns `False` for any module whose source lives under
`~/.cache/ihc/sources/` or `$IHC_NIX_SOURCE_DIR` — every Hackage package.
`hspec-expectations` and `hspec-core` are in those directories, so they're never
loaded when first needed. `resolveImport` falls through to `tryImports rest`,
eventually returns `Nothing`, and `discoverInModule` silently assumes the name
is a builtin (line 1085-1088: "Assume a builtin; let the evaluator complain").

**Why import-only probes pass:** The module header is parsed to build the import
map, but no body is loaded. `main = putStrLn "ok"` has no free vars from
`Test.Hspec`, so `discoverInModule` never needs to resolve those symbols.

**Fix direction:** Differentiate "don't eagerly walk base/ghc-internal" from
"don't load small, direct Hackage leaf packages." The most targeted fix:
allow loading any module that is directly `import`ed in the **entry module**
(depth=0), while retaining the skip guard for modules reached only transitively
during free-var discovery. This is a ~50 LOC change in `resolveImport`.

---

## Blocker #2 (Moderate): `ansi-terminal`, `haskell-lexer`, `quickcheck-io` not in nix bundle

**Severity:** Blocks `hspec`/`hspecResult` execution once blocker #1 is fixed.

`Test.Hspec.Core.Runner` imports `System.Console.ANSI` (`ansi-terminal`).
`Test.Hspec.Core.Formatters.Pretty.Parser` imports `Language.Haskell.Lexer`
(`haskell-lexer`). `Test.Hspec.Core.QuickCheck.Util` imports `Test.QuickCheck.IO`
(`quickcheck-io`). None of these appear in `$IHC_NIX_SOURCE_DIR` or the user cache.

**Fix:** Add `ansi-terminal`, `haskell-lexer`, `quickcheck-io` to the flake's
hackage-sources bundle. All are pure Haskell; no new primops needed.
`ansi-terminal` uses `hPutStr`/`hPutChar` which are already in builtins.

---

## Blocker #3 (Moderate): ST monad (`Control.Monad.ST`, `Data.STRef`) not implemented

**Severity:** Blocks `Test.Hspec.Core.Runner` and `Test.Hspec.Core.Shuffle` once
blockers 1-2 are fixed.

`Test.Hspec.Core.Runner` and `Test.Hspec.Core.Shuffle` use `runST`, `stToIO`,
`newSTRef`, `readSTRef`, `writeSTRef`, `modifySTRef`. IHC has no ST monad support
at all: no `VSTRef` value, no `runST` primop, no class instances.

`Control.Monad.ST` re-exports from `Control.Monad.ST.Imp` which calls `GHC.ST.*`
primops (`State# RealWorld`, `unsafeCoerce#`). The base source IS present
(`~/.cache/ihc/sources/base-4.19.0.0/Control/Monad/ST.hs`) but it hits primops
IHC doesn't map.

**Fix direction:** Implement `ST s a` as `IORef`-backed values (since IHC is
single-threaded, ST and IO are semantically equivalent). Add host-backed `runST`,
`newSTRef`, `readSTRef`, `writeSTRef` builtins using `IORef` under the hood.
This is a ~100-200 LOC change in `Builtins.hs` plus a new `Val` constructor.

---

## Comparison with Prior Probes

| Issue | Aeson | Warp | **Hspec** |
|-------|-------|------|-----------|
| Load guard blocks Hackage imports | Partially | Yes | **Yes — primary failure** |
| Named re-export not followed through intermediary | Yes (critical) | Yes | Not yet reached |
| Constructor re-export `(..)` broken | Yes | Yes | Not yet reached |
| Missing nix bundle packages | No | Partially | **Yes (3 packages)** |
| ST monad unimplemented | No | No | **Yes — new gap** |
| FFI / C allocator in hot path | Yes | Yes | Unlikely in basic path |

**Novel gaps vs aeson/warp:**
1. The ST monad requirement (`runST`/`STRef`) is new — not surfaced before.
2. Three missing nix-bundle packages (`ansi-terminal`, `haskell-lexer`,
   `quickcheck-io`) — aeson/warp happened to have all deps bundled.
3. `hspec-expectations` (the shallow layer) has the simplest dependency tree
   of any package tested so far: only `HUnit` + `call-stack` + base. It should
   be the quickest win once the load guard is fixed.

---

## Realistic Loadability Assessment

**`shouldBe` / `shouldSatisfy` (hspec-expectations only):** 1 fix away (load guard).
This is the layer our own test fixtures use most.

**`hspec $ describe $ it $ shouldBe` (full runner):** 3 fixes away:
load guard + missing nix packages + ST monad.

The loader-hardening work (heap-overflow bug) is orthogonal but relevant: hspec's
runner spawns concurrent test threads, which may stress the same allocator path.
Once the heap-overflow fix lands, revisit blocker #3 — the parallel `Eval` module
uses `forkIO` which IHC already handles.

**Recommended priority:**
1. Fix load guard in `resolveImport` (unblocks `shouldBe`, likely also re-activates
   partial aeson and warp paths — highest ROI single change).
2. Add 3 packages to nix bundle (trivial, flake.nix only).
3. Implement ST monad as IORef-backed builtins.

---

## Files Referenced

- `/Users/marc/digitallyinduced/interactive-haskell-computer/src/IHC/Scheduler.hs`
  — `resolveImport` (line ~1116), `isLocalModule` (line ~1015), `discoverInModule` (line ~1032)
- `/Users/marc/digitallyinduced/interactive-haskell-computer/src/IHC/Builtins.hs`
  — ST monad not present; needs addition
- `/nix/store/2xprchszvrbjapahvmf3w6pzmzqpgvpm-ihc-hackage-sources/hspec-expectations-0.8.4/`
- `/nix/store/2xprchszvrbjapahvmf3w6pzmzqpgvpm-ihc-hackage-sources/hspec-core-2.11.16/`
- `/Users/marc/.cache/ihc/sources/hspec-2.11.17/src/Test/Hspec.hs`
- `/Users/marc/.cache/ihc/sources/base-4.19.0.0/Control/Monad/ST.hs`
