# Dead-code findings — 2026-05-09, against master `92f78b6`

One-shot dead-code audit against current master. Two confidence tiers fixed in this PR.

The Tier-C scan was run by temporarily flipping `ihc.cabal:13` from `-Wno-unused-imports` to `-Wunused-imports`, doing `cabal build all`, capturing the log, and reverting. The original line was kept verbatim during diagnosis; the final commit re-tightens to `-Werror=unused-imports` to match the existing `-Werror=unused-binds` posture.

## A. Confirmed dead — DELETED in this PR

### A1. Orphan probe scratchpads (`probe/`)

Seven `.hs` files in `probe/` with **zero filename references** anywhere in the tree (`*.hs`, `*.cabal`, `*.nix`, `Makefile`, `*.sh`, `*.md`, `*.yml`). The two probes wired into `ihc.cabal` (`ParseProbe.hs`, `HsxProbe.hs`) stay.

| File | Status |
|---|---|
| `probe/Hello.hs` | zero refs |
| `probe/ForeignMemchrProbe.hs` | zero refs |
| `probe/ParseHeaderLinesProbe.hs` | zero refs |
| `probe/S1_WithForeignPtrEmpty.hs` | zero refs |
| `probe/S2_WithForeignPtrPlusPeek.hs` | zero refs |
| `probe/S3_WithForeignPtrMemchr.hs` | zero refs |
| `probe/S4_PSDestructure.hs` | zero refs |

Recipe: `grep -rnE "<basename>\.hs|probe/<basename>\b" --include='*.hs' --include='*.cabal' --include='*.nix' --include='Makefile' --include='*.sh' --include='*.md' --include='*.yml' . | grep -v "^\./probe/<basename>\.hs:"` returns nothing for each.

### A2. Orphan finding doc

| File | Status |
|---|---|
| `reprobe-findings.md` | zero refs |

The other `*-findings.md` files at repo root are referenced 3–10 times each from test fixtures as paper trail. Only `reprobe-findings.md` is unreferenced.

## C1. Redundant imports — DELETED in this PR

27 unused imports surfaced by GHC `-Wunused-imports`. All deleted; build passes with the warning elevated to `-Werror=unused-imports` in the final commit of this PR.

| File:Line | Import |
|---|---|
| `src/IHC/CabalProject.hs:54` | `isSuffixOf` from `Data.List` |
| `src/IHC/CabalProject.hs:56` | qualified `Data.Map.Strict` |
| `src/IHC/CabalProject.hs:86` | `library` from `Distribution.PackageDescription` |
| `src/IHC/CabalProject.hs:93` | qualified `Distribution.ModuleName` |
| `src/IHC/Cpp.hs:52` | `isSpace` from `Data.Char` |
| `src/IHC/PackageStore.hs:42` | `getDirectoryContents` from `System.Directory` |
| `src/IHC/PackageStore.hs:45` | `takeExtension` from `System.FilePath` |
| `src/IHC/Pretty.hs:36` | `chr` from `Data.Char` |
| `src/IHC/ModuleHeader.hs:44` | qualified `Data.ByteString` |
| `src/IHC/ModuleHeader.hs:46` | `Data.Word` |
| `src/IHC/ModuleHeader.hs:47` | `</>` from `System.FilePath` |
| `src/IHC/TypeAST.hs:23` | `Data.ByteString` |
| `src/IHC/TypeUnify.hs:16` | `Data.ByteString` |
| `src/IHC/TypeUnify.hs:19` | `Data.Map.Strict` |
| `src/IHC/Elaborate.hs:34` | `lookupInstance` from `IHC.Classes` |
| `src/IHC/Eval.hs:33` | `Data.Int` |
| `src/IHC/FFI.hs:69` | qualified `System.IO` |
| `src/IHC/Scan.hs:73` | qualified `System.IO` |
| `src/IHC/Scan.hs:78` | `Debug.Trace` |
| `src/IHC/Builtins.hs:31` | `STM, modifyTVar, modifyTVar', newTVar, orElse` from `Control.Concurrent.STM` |
| `src/IHC/Builtins.hs:35` | `GHC.Conc.Sync` |
| `src/IHC/Builtins.hs:40` | `Exception, IOException` from `Control.Exception` |
| `src/IHC/Builtins.hs:71` | `exitWith` from `System.Exit` |
| `src/IHC/Scheduler.hs:74` | `cachedPackageSearchPath` from `IHC.CabalProject` |
| `src/IHC/Scheduler.hs:96` | `catalogueHasClass` from `IHC.Classes` |
| `src/IHC/Scheduler.hs:114` | qualified `IHC.TypeAST` |
| `probe/ParseProbe.hs:13` | `Exception` from `Control.Exception` |

## Verification

- `cabal build all` passes after each commit.
- After the C1 cleanup commit + flag-flip + rebuild: **0** for `-Wunused-imports / -Wunused-top-binds / -Wunused-local-binds / -Wunused-binds`.
- The final commit elevates `-Wunused-imports` to `-Werror=unused-imports` so regressions break CI.

## Re-running the scan

```sh
# Edit ihc.cabal:13 — flip warnings:
#   FROM: -Wno-unused-imports
#   TO:   -Wunused-imports
direnv exec . cabal clean
direnv exec . cabal build all 2>&1 | tee /tmp/dead-code-warnings.log
git checkout -- ihc.cabal
grep -nE "warning: \[GHC-[0-9]+\] \[-Wunused-(imports|top-binds|local-binds)\]" /tmp/dead-code-warnings.log
```

## Out of scope

- Does not touch `-Wno-unused-matches` / `-Wno-name-shadowing` (project policy: too noisy to lift).
- Does not delete the other `*-findings.md` docs at repo root — referenced 3-10x each from test fixtures. Only `reprobe-findings.md` (zero refs) was removed.
