# Aeson Source-Load Diagnostic Findings

**Date:** 2026-04-16  
**IHC version:** 0.2.0.0 (Phase 2.0 — tree-walking evaluator)  
**Aeson version:** 2.2.3.0 (source at `~/.cache/ihc/sources/aeson-2.2.3.0/`)

---

## Probe Matrix

| Probe | Program | Result |
|-------|---------|--------|
| 1 | `import qualified Data.Aeson as A; main = putStrLn "import ok"` | **PASS** |
| 1b | `A.Number 42` (Value constructor through qualified import) | **FAIL** – `unbound variable A.Number` |
| 2 | `A.encode (1 :: Int)` | **FAIL** – `unbound variable encodingToLazyByteString` |
| 3 | `A.encode ([1,2,3] :: [Int])` | **FAIL** – same as probe 2 |
| 4 | `A.decode (BL.pack "42") :: Maybe Int` | **PASS** (exits 0; decode path not exercised) |
| 5a | `data Person deriving Generic; instance ToJSON/FromJSON; print p` | **PASS** (print works) |
| 5b | same + `A.encode p` | **FAIL** – same as probe 2 |

---

## Blocker #1 (Critical): Import Re-export Chain Not Followed for Named Exports

**Severity:** Blocks ALL encoding paths.  
**Error:** `unbound variable encodingToLazyByteString`

### Root cause

`Data.Aeson.encode` is defined as:
```haskell
-- Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString)
encode = encodingToLazyByteString . toEncoding
```

`Data.Aeson.Encoding` re-exports `encodingToLazyByteString` via an **explicit named export** (not a `module X` re-export):
```haskell
-- Data.Aeson.Encoding
module Data.Aeson.Encoding
    ( encodingToLazyByteString  -- explicit name
    , ...
    ) where
import Data.Aeson.Encoding.Internal   -- where the function is actually defined
```

The IHC scheduler's `resolveImport` logic handles re-exports only when the intermediary module uses the `module Foo` syntax in its export list (tracked as `ExportModule`). When `findOrResolveLhs` returns `Nothing` for the intermediary module, `followModuleReexports` is called — but it only chases `ExportModule` entries. Since `Data.Aeson.Encoding` has no `module Data.Aeson.Encoding.Internal` in its export list, the chase stops empty-handed.

**Fix needed (`Scheduler.hs` near `resolveImport`):** When `mLhs = Nothing` for a target module that exports `name` via `ExportName`, also recursively search that module's imports for the definition. This is a "follow unqualified imports through named re-exports" step.

**Affected functions:** `encodingToLazyByteString`, and transitively all functions exported from gateway modules like `Data.Aeson.Encoding`, `Data.ByteString.Builder`, etc.

---

## Blocker #2 (Critical): Constructor Re-export via `ExportType(..)` Not Resolved

**Severity:** Blocks all constructor usage through qualified imports.  
**Error:** `unbound variable A.Number`

### Root cause

`Data.Aeson` exports `Value(..)`:
```haskell
module Data.Aeson ( Value(..), ... )
-- Value and its ctors (Object, Array, String, Number, Bool, Null) defined in Types.Internal
```

In `exportsName` (`Scheduler.hs`):
```haskell
matchExport (ExportType m _)  = n == m
```

This matches only `n == "Value"`, never `n == "Number"`. So the scheduler finds `ExportType "Value" (Just [])` in Data.Aeson's export list but returns `False` for `"Number"`, and the constructor is left unresolved.

**Fix needed (`Scheduler.hs` near `exportsName` / `resolveImport`):** For `ExportType name (Just subs)`, match any name in `subs`. For `ExportType name (Just [])` (the `(..)` form), look up all constructors of `name` from the module's `DataRegistry` and accept any of them.

---

## Blocker #3 (Moderate): `Data.ByteString.Builder` FFI Declarations

**Severity:** Will block `toLazyByteString` (called by `encodingToLazyByteString`) once blockers 1-2 are fixed.

`Data.ByteString.Builder.Internal` (the actual home of `toLazyByteString`) uses pinned `ByteArray#` allocation and `newPinnedByteArray#` primops. Additionally, `Data.ByteString.Internal.Type` has `foreign import ccall` for C functions (`c_strlen`, `c_memcmp`, `fps_reverse`, etc.) that the interpreter has no runtime mechanism to dispatch.

`toLazyByteString` is a legitimate builtin candidate (it touches C allocation via pinned ByteArrays — no pure Haskell can implement the allocator). It should be added to the builtin env with a host-backed implementation.

---

## Novel Language Features Surfaced

### Template Haskell (TemplateHaskellQuotes)
- `Data.Aeson.Types.Internal` uses `instance TH.Lift Value` with TH quote syntax
- `Data.Aeson.TH` is the full TH-based derive module
- IHC's `Language.Haskell.TH.*` stub handles the import path but full splice execution is not exercised in the basic encode path

### Type Families (Associated)
- `Data.Aeson.KeyMap`: `instance GHC.Exts.IsList (KeyMap v) where type Item (KeyMap v) = (Key, v)`
- No type-family mechanism in IHC yet; will be hit when `KeyMap` operations are exercised

### DerivingVia
- `Data.Aeson.Types.ToJSON` and `FromJSON` use `deriving via` extensively:
  ```
  deriving via (a :: Type) instance ToJSON a => ToJSON (Identity a)
  deriving via Identity instance ToJSON1 Down
  ```
- `DerivingVia` not in IHC parser/evaluator

### MagicHash / UnboxedTuples
- `containers-0.6.7` (`Data.Map.Internal`, `Data.IntMap.Internal`) uses `MagicHash`
- `scientific-0.3.7.0` uses `UnboxedTuples`
- These will surface when `Data.Map` (used in `KeyMap`) and `Scientific` (used in `Value`) are exercised

### Constraint Solving Depth
- Generic-based instances (`GToJSON`, `GFromJSON`) involve deep type-class stacking: `Generic`, `Rep`, product/sum type recursion plus wrapper classes
- IHC's class-dispatch handles `deriving Generic` and basic instances (probe 5a passes); the full generic-to-JSON dispatch chain is unverified

---

## Assessment: Is Aeson Realistically Loadable Next Sprint?

**Short answer: Partially — the import-only and record-construction paths work today; encoding requires fixing 2 scheduler bugs.**

- **Blockers 1+2 (re-export + constructor resolution)** are self-contained scheduler changes (~100-200 LOC each in `Scheduler.hs`). Fixing them benefits ALL multi-layer re-exporting packages, not just aeson. These are the right next-sprint items.
- **Blocker 3 (ByteString.Builder FFI)** requires builtin-backing `toLazyByteString`; this is a legitimate exception to the no-shim rule (the allocator is RTS-exclusive).
- **Type families, DerivingVia** are medium-term work; not hit in basic `encode Int` path.
- **Full decode + attoparsec** introduce their own parsing complexity (attoparsec is pure Haskell — only issue is Parser monad depth and partial parsing primitives).

**Realistic next-sprint target:** Fix blockers 1+2 → `A.encode (1 :: Int)` and `A.encode (Just 1)` should work. Record `ToJSON` via generics is reachable once constraint-solving depth is sufficient.

---

## Files Referenced

- `/Users/marc/digitallyinduced/interactive-haskell-computer/src/IHC/Scheduler.hs` — `resolveImport`, `exportsName`, `followModuleReexports`, `moduleReexports`
- `/Users/marc/digitallyinduced/interactive-haskell-computer/src/IHC/ModuleHeader.hs` — `parseExportList`, `ExportType`
- `~/.cache/ihc/sources/aeson-2.2.3.0/src/Data/Aeson.hs`
- `~/.cache/ihc/sources/aeson-2.2.3.0/src/Data/Aeson/Encoding.hs`
- `~/.cache/ihc/sources/aeson-2.2.3.0/src/Data/Aeson/Encoding/Internal.hs`
- `~/.cache/ihc/sources/aeson-2.2.3.0/src/Data/Aeson/Types/Internal.hs`
- `~/.cache/ihc/sources/bytestring-0.12.2.0/Data/ByteString/Builder/Internal.hs`
