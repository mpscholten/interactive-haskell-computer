# Re-Probe Library Source-Loads + XFAIL Graduation Findings

**Date:** 2026-04-16  
**IHC HEAD at probe time:** c746e03 (Lexer: demote 'as' to soft keyword)  
**Previous probe baselines:** aeson-dryrun-findings.md, hspec-dryrun-findings.md, bytestring-0.12.2.0/

---

## Task A — Library Source-Load Re-Probe

### Probe Matrix

| Library | Probe program | Result | Blocker |
|---------|---------------|--------|---------|
| bytestring | `BC.pack "Hello"` → `BC.putStrLn` | **FAIL** | Named re-export chain: `BC.putStrLn` / `packChars` not followed through `Data.ByteString.Char8` import-and-re-export pattern |
| hspec | `import Test.Hspec; main = putStrLn "ok"` | **PASS** | — (import-only, was already passing) |
| aeson | `import qualified Data.Aeson as A; main = putStrLn "ok"` | **PASS** | — (import-only, was already passing) |
| text | `import qualified Data.Text as T; main = putStrLn "ok"` | **PASS** | — (import-only, was already passing) |
| Data.List | `import Data.List; main = print (sort [3,1,2])` | **PASS** | Previous blocker (`TkAs` in `import qualified GHC.List as List` in transitive deps) **FIXED** by `c746e03` |

### Bytestring — Blocker Detail

Previous error: `unbound variable BC.putStrLn`  
Current error: `unbound variable packChars`

Progress: the re-export chain now resolves `BC.putStrLn` to `hPutStrLn stdout` in `Data.ByteString.Char8`. The remaining error is one layer deeper — `packChars` lives in `Data.ByteString.Internal.Type` and is imported into `Data.ByteString.Char8` via an explicit named import list (`import Data.ByteString (null,length,...)`), then re-exported through the module's own export list. The scheduler's `resolveImport` / `followModuleReexports` only chases `ExportModule` re-exports, not named imports that effectively re-export. Fix direction: the named re-export resolver in Scheduler needs to walk the import list of the intermediary module when `mLhs` is `Nothing` and the name appears in the module's export list.

### Data.List — New PASS (was FAIL)

Previous error: `parse error at Data/OldList.hs:7:18 expected = or | in binding; saw TkAs`

Root cause: `Data.OldList`'s transitive dependencies (e.g. `Data.Bits`, `Data.Semigroup`) use `import qualified GHC.List as List`. The old lexer tokenised `as` as a hard keyword `TkAs` even inside import declarations, which the parser treated as unexpected in a binding context.

Fix (`c746e03`): `as` is now a soft keyword — lexed as `TkIdent "as"` in most positions and only promoted to `TkAs` inside the module-header import/export context. `Data.List` / `OldList` now loads and `sort` works end-to-end.

---

## Task B — XFAIL Graduation

### Graduated (4 of 7 total XFAIL files)

| Fixture (old name) | New name | Previously blocked by | Fixed by |
|--------------------|----------|-----------------------|----------|
| `bang_pattern_strict_XFAIL` | `bang_pattern_strict` | `[1..10]` ArithSeq / enumFromTo + bang patterns | ArithSeq parsing + bang pattern support landed earlier |
| `ops_fixity_prec_XFAIL` | `ops_fixity_prec` | `^` operator not in builtins | `^` was added to Builtins |
| `io_file_roundtrip_XFAIL` | `io_file_roundtrip` | `writeFile`/`readFile` not in builtins | `writeFile`/`readFile` wrappers added to Builtins |
| `typeapp_promoted_XFAIL` | `typeapp_promoted` | `@'True` promoted-tick lexed as char literal `'T'` + leftover ident | Parser/lexer now skips type-app arguments correctly; `@'Con` handled |

All 4 pass their expected `.out` files exactly. Both the explicit `RunFile.hs` tests and auto-discovery suite confirm green.

### Remaining XFAILs (3 of 7)

| Fixture | Blocker | Notes |
|---------|---------|-------|
| `guards_multiclause_where_XFAIL` | Eval: `unbound variable bmiVal` in guarded multi-clause function's `where` binding; also float literals (`18.5` etc.) not yet supported | The `ce01b91` commit fixed guarded+pattern *let*-bindings but NOT `where` bindings on guarded multi-clause functions. The `where` env isn't threaded into the guard evaluation context. This is a distinct eval bug. |
| `io_handle_write_XFAIL` | Parse error at `GHC/IO/Handle/FD.hs:22:9 unexpected token; saw TkRParen` | Line 22 is ` ) where` — the closing paren of the module export list. The parser cannot handle multi-line module export lists that begin with `module Foo.Bar.Baz (`. |
| `parse_error_position_XFAIL` | **Not a failure** — this fixture is hardcoded in `RunFile.hs` (line 748) as a deliberate parse-error test. It must keep the `_XFAIL` suffix because `RunFile.hs` references it by that name. The test passes (reports `:2:` in error message as expected). |

---

## Stuck XFAILs that recent commits SHOULD have fixed — investigate

### `guards_multiclause_where_XFAIL` — `ce01b91` did NOT fully fix

`ce01b91` ("Parser: guarded + pattern let-bindings, multi-binding layout let") fixed let-bindings. However this fixture uses a **where** clause on a **guarded multi-clause top-level function**:

```haskell
bmi weight height
    | bmiVal <= 18.5 = "underweight"
    ...
  where
    bmiVal = weight / (height * height)
```

The evaluator fails with `unbound variable bmiVal` — meaning the `where` bindings from a guarded function are not being added to the environment before guards are evaluated. This is a separate eval path from let-bindings.

Secondary blocker: float literals (`18.5`, `25.0`, `30.0`) are still not supported in the evaluator (no `VFloat` in Val.hs). Even if `bmiVal` is fixed, the fixture will still fail on float comparison.

### `io_handle_write_XFAIL` — `e554b70` GHC-include fallback did NOT fully fix

`e554b70` ("CPP: add GHC installed-include fallback") was expected to resolve `HsBaseConfig.h` / `MachDeps.h` skips in `System.IO`'s dependency chain. These are now just warnings ("IHC.Cpp: skipping unresolvable system include"). The real blocker is downstream: `GHC/IO/Handle/FD.hs` has a multi-line module export list `module GHC.IO.Handle.FD (\n  stdin, stdout, ...\n ) where` and the parser fails on the `) where` token at position 22:9.
