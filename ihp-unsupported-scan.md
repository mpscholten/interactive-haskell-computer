# IHP Gap Analysis: Features Not Yet on `ihc` Roadmap

**Survey date:** 2026-04-15  
**IHP path:** `/Users/marc/digitallyinduced/ihp/`  
**IHP commit:** `fd2f5f70` (Merge pull request #2205)  
**Scope:** All `.hs` files in IHP excluding `dist-newstyle/`

Note: A prior survey (`ihp-feature-survey.md`) already covers TH, type families, DataKinds, and DerivingVia. This scan focuses on the **surprises** — things NOT mentioned in that survey or the current roadmap.

---

## Table 1: Language Pragmas — Full Count vs. Roadmap Status

All pragmas found via `grep -rh "{-# LANGUAGE"` across IHP source, sorted by count.

| Pragma | Count (files) | Roadmap Status | Notes |
|--------|---------------|----------------|-------|
| `CPP` | 61 | Supported (hand-rolled) | — |
| `AllowAmbiguousTypes` | 55 | **MISSING** | Critical — all `@type`-application dispatch functions use this |
| `OverloadedStrings` | 47 | Designed (Phase 2.6) | — |
| `UndecidableInstances` | 45 | **MISSING** | Required by nearly all MPTC + type family interplay |
| `TypeFamilies` | 39 | Designed (Phase 3.2) | — |
| `NoRebindableSyntax` | 32 | **MISSING** | `GHC2021` default enables `RebindableSyntax`; IHP explicitly turns it off |
| `BangPatterns` | 31 | Supported (Phase 2.6) | — |
| `ScopedTypeVariables` | 30 | **MISSING** | Required for `forall a.` in function bodies |
| `ForeignFunctionInterface` | 28 | Not in IHP source proper (only dist-newstyle deps) | — |
| `CApiFFI` | 28 | Not in IHP source proper (only dist-newstyle deps) | — |
| `DataKinds` | 27 | Designed (Phase 3.4) | — |
| `FlexibleInstances` | 26 | **MISSING** | Needed for `instance Foo [a]`, `instance Bar (Maybe Int)` etc. |
| `FlexibleContexts` | 26 | **MISSING** | Needed for constraints like `(Eq a, Show a) => ...` in complex positions |
| `ConstraintKinds` | 22 | **MISSING** | Used to name constraint synonyms: `type ModelConstraints a = (Eq a, Show a, ...)` |
| `MultiParamTypeClasses` | 20 | Partially designed (Phase 2.6) | Multi-param class dispatch is not fully designed |
| `PolyKinds` | 19 | **MISSING** | Required by `QueryBuilder (table :: Symbol)` and many PolyKind-indexed classes |
| `TypeOperators` | 18 | Designed (Phase 3.4) | — |
| `TypeApplications` | 17 | **MISSING** — partially | `@Type` at value level (`TypeMap.lookup @Foo`) is not designed |
| `GADTs` | 16 | Designed (Phase 2.9.5) | — |
| `StandaloneDeriving` | 13 | **MISSING** | `deriving instance Eq (Foo a)` outside data declaration |
| `InstanceSigs` | 13 | **MISSING** | Type signatures inside `instance` blocks |
| `FunctionalDependencies` | 12 | **MISSING** | `class C a b | a -> b where ...` |
| `IncoherentInstances` | 10 | **MISSING** | IHP uses it in param parsing, model support, form fields |
| `DeriveAnyClass` | 10 | **MISSING** | `deriving (FromJSON, ToJSON, NFData)` with zero-method classes |
| `TemplateHaskell` | 8 | Designed (Phase 3.1) | — |
| `QuasiQuotes` | 8 | Designed (Phase 3.1) | — |
| `DeriveDataTypeable` | 8 | **MISSING** | `deriving (Data, Typeable)` for older code |
| `NoImplicitPrelude` | 7 | **MISSING** | Files explicitly opt out of Prelude; IHP uses custom prelude |
| `GeneralizedNewtypeDeriving` | 7 | Designed (Phase 3.3) | — |
| `NamedFieldPuns` | 6 | **MISSING** | `Foo { x }` instead of `Foo { x = x }` in patterns |
| `EmptyDataDecls` | 6 | **MISSING** | `data Foo` (no constructors) as phantom type |
| `PackageImports` | 5 | **MISSING** | `import "interpolate" Data.String.Interpolate (i)` |
| `TypeSynonymInstances` | 5 | **MISSING** | `instance Foo String where ...` |
| `ImplicitParams` | 5 | Designed (Phase 3.6) | `?context :: ControllerContext` — IHP's entire context system |
| `RankNTypes` | 4 | **MISSING** | `forall a. ... -> IO a` in TransactionRunner |
| `ExistentialQuantification` | 3 | **MISSING** | `data SomeView = forall a. View a => SomeView a` |
| `BlockArguments` | 3 | Supported | — |
| `ApplicativeDo` | 3 | **MISSING** | Used in IHP.FetchPipelined for DB query pipelining |
| `ViewPatterns` | 2 | **MISSING** | `f (view -> x) = ...` |
| `TupleSections` | 2 | **MISSING** | `(1, , 3)` partial tuple syntax |
| `ConstrainedClassMethods` | 2 | **MISSING** | Class methods with constraints on the class variable |
| `QuantifiedConstraints` | 1 | **MISSING** | `forall a. C a => D a` inside constraints |
| `OverloadedLabels` | 1 | Designed (Phase 3.5) | `#fieldName` for `Proxy name` — IHP's primary field-access syntax |
| `NoMonomorphismRestriction` | 1 | **MISSING** | Turns off monomorphism restriction |
| `NoFieldSelectors` | 1 | **MISSING** | Prevents record field names becoming functions |
| `NamedDefaults` | 1 | **MISSING** | GHC 9.12+ feature for named default declarations |
| `LambdaCase` | 1 | Supported | — |
| `DeriveFunctor` | 1 | **MISSING** | `deriving Functor` |
| `DefaultSignatures` | 1 | **MISSING** | Default method type signatures for Generic-based deriving |
| `DuplicateRecordFields` | cabal only | **MISSING** | Multiple records with same field name |
| `DisambiguateRecordFields` | cabal only | **MISSING** | Resolves ambiguous field names using type info |
| `OverloadedRecordDot` | cabal only | Designed (agent in flight) | — |

---

## Table 2: Syntactic Forms — IHP Usage with Examples

| Syntactic Form | Example (file:line) | IHP Count | ihc Status |
|---------------|---------------------|-----------|------------|
| **TypeApplications `@T`** | `TypeMap.lookup @value customFields` (`ihp-context/IHP/ControllerContext.hs:139`) | ~300+ | **MISSING** — at value level, `@T` in expression position is never parsed |
| **ImplicitParams `?name`** | `let ?modelContext = modelContext` (`ihp-typed-sql/Test/TypedSqlSpec.hs:312`), `?context :: ControllerContext` everywhere | 677 accesses | Designed (Phase 3.6) |
| **OverloadedLabels `#field`** | `filterWhere (#email, "foo@example.com")` (`ihp-ide/Test/SchemaCompilerSpec.hs:286`) | ~446 occurrences | Designed (Phase 3.5) |
| **`forall a.` rank-N types** | `runInTransaction :: forall a. HasqlSession.Session a -> IO a` (`ihp/IHP/ModelSupport/Types.hs:73`) | 30+ | **MISSING** — type sig parser does not handle inner `forall` |
| **Existential data** | `data SomeView = forall a. (View a) => SomeView a` (`ihp-job-dashboard/IHP/Job/Dashboard/View.hs:22`) | 3 | **MISSING** |
| **ScopedTypeVariables** | `let action :: forall a. IO a = ...` | 30+ files | **MISSING** |
| **`NamedFieldPuns`** | `let Foo { x } = record` (`ihp-typed-sql/IHP/TypedSql/TypeMapping.hs:1`) | 6 files | **MISSING** |
| **PackageImports** | `import "interpolate" Data.String.Interpolate (i)` (`ihp-schema-compiler/IHP/SchemaCompiler.hs:20`) | 1 explicit | **MISSING** — parser does not support package-qualified import syntax |
| **`ViewPatterns`** | `(view -> x)` pattern syntax (`ihp-hsx/parser/IHP/HSX/HsExpToTH.hs:1`) | 1 file | **MISSING** |
| **`TupleSections`** | `(, x)` partial tuple application | 2 files | **MISSING** |
| **`StandaloneDeriving`** | `deriving instance Eq (Foo a)` | 13 files | **MISSING** |
| **`ApplicativeDo` desugaring** | `do { x <- fetchUsers; y <- fetchPosts; pure (x, y) }` desugars to `liftA2` (`ihp/IHP/FetchPipelined.hs:1`) | 3 files | **MISSING** — `do`-notation desugaring always monadic |
| **`DeriveAnyClass`** | `deriving (NFData, FromJSON)` with zero-overhead derivation | 10 files | **MISSING** |
| **`InstanceSigs`** | `instance Foo Bar where { foo :: Bar -> Int; foo = ... }` | 13 files | **MISSING** |
| **`DefaultSignatures`** | `class C a where { default selectLabel :: Show a => a -> Text }` (`ihp/IHP/View/Form/Select.hs:176`) | 3 sites | **MISSING** |
| **`ConstraintKinds` synonyms** | `type IncludeConstraints model = (...)` | 22 files | **MISSING** |
| **`FunctionalDependencies`** | `class SetField name model value | field model -> value where` (`ihp/IHP/Record.hs:77`) | 12 files | **MISSING** |
| **Type-level `TypeError`** | `instance (TypeError ('Text "Use 'param'...")) => ParamReader (IO param)` | 10 sites | **MISSING** — type-level errors are compile-time; ihc optimistic skip works here |
| **`QuantifiedConstraints`** | `forall a. C a => D a` inside constraints | 1 file | **MISSING** |

---

## Table 3: Non-Trivial Dependencies

| Package | TH? | Type Families? | Unusual extensions | Interpreter notes |
|---------|-----|----------------|-------------------|-------------------|
| `typerep-map` | No | Yes (internally) | `PolyKinds`, `TypeApplications` | Core to IHP's context system; `TypeMap.lookup @Foo` uses `TypeApplications` at call site; the library itself uses `unsafe` operations and `GHC.Exts` |
| `classy-prelude` | No | Minor | `OverloadedStrings`, `MonoTraversable` | Shadows many Prelude names; `MonoFoldable` requires multi-param type classes |
| `hasql` / `hasql-pool` | No | No | `RankNTypes` in session API | No TH; the `Session a` and `Statement a b` types use rank-N quantification |
| `aeson` | Conditional | No | Heavy `Generic` deriving | IHP uses `Data.Aeson.TH` in only 14 splices; `deriving (Generic, FromJSON)` is the non-TH path |
| `haskell-src-exts` + `haskell-src-meta` | Yes | No | Uses GHC API indirectly | Needed for `[hsx|...|]` expression parsing inside QQ; deep rabbit hole to interpret |
| `blaze-html` / `blaze-markup` | No | No | `OverloadedStrings` | Clean to interpret; just string builder combinators |
| `neat-interpolation` | Yes (QQ) | No | `QuasiQuotes` | `[trimming|...|]` and `[text|...|]` QQs used in schema compiler and tests |
| `interpolate` | Yes (QQ) | No | `QuasiQuotes`, `PackageImports` | `[i|...|]` string interpolation; `import "interpolate" Data.String.Interpolate` |
| `wai` / `warp` | No | No | — | Clean; no unusual extensions |
| `postgresql-simple` | No | No | `TemplateHaskell` optional | The `sql` QQ in pg-simple is optional; IHP's hasql backend doesn't use it |
| `mtl` / `transformers` | No | Yes (minor) | `RankNTypes` | `StateT`, `ReaderT`, `WriterT` all use rank-N in their bind/run operations |
| `unliftio` | No | No | `RankNTypes` | `withRunInIO :: (UnliftIO m -> IO a) -> m a` is rank-2 |
| `mono-traversable` | No | Yes | `MultiParamTypeClasses` | `MonoFoldable`, `MonoTraversable` defined with type families and MPTC |
| `lens` | No | Yes (via `Getter`, `Setter`) | `RankNTypes`, `TypeFamilies` | IHP uses `Control.Lens hiding ((|>), set)` in one file only; moderate usage |
| `websockets` | No | No | — | Clean; used for DataSync WebSocket connections |
| `minio-hs` | No | No | — | S3 storage; clean API |

---

## Surprises Section

### Surprise 1: `ImplicitParams` — IHP's ENTIRE context system (Phase 3.6 underestimates scope)
Now on the roadmap as Phase 3.6, but the depth is not fully appreciated. IHP threads ALL request-level state through implicit parameters:
- `?context :: ControllerContext` — the WAI request, session, flash messages, etc.
- `?modelContext :: ModelContext` — the database pool and transaction runner
- `?action :: controller` — the current controller action
- `?view :: view` — the current view being rendered
- `?schema :: Schema` and `?compilerOptions :: CompilerOptions` — in schema compiler

There are **677 sites** in IHP source accessing implicit params, 252 sites binding them with `let ?name = value`. Every single controller action and view has these constraints in its type. Without ImplicitParams, no IHP app code can run. Current ihc does not parse `?name` in type signatures, does not handle `let ?name = value` binding, and does not thread implicit params through function calls.

### Surprise 2: `OverloadedLabels` — deeper than it looks (Phase 3.5)
Now on the roadmap as Phase 3.5, but the scope is larger than the entry suggests. Every IHP DB query uses `#fieldName` syntax: `filterWhere (#email, "foo")`, `orderByDesc #createdAt`, `set #fieldName value`. There are ~446 occurrences across IHP source. The `#field` token parses as `fromLabel @"field"` which resolves to `Proxy @"field"` via `IHP.HaskellSupport`'s `IsLabel` instance. The lexer needs to handle `#word` as a label literal (separate from `MagicHash`) AND the evaluator needs to dispatch through `IsLabel` class. Both are non-trivial.

### Surprise 3: `AllowAmbiguousTypes` + `TypeApplications` at value level (55 files)
IHP uses a pattern where functions like `TypeMap.lookup @Foo` don't have the type variable in the return/arg position — `AllowAmbiguousTypes` lets GHC accept this, and `TypeApplications` supplies the missing type. This combo appears in 55 files and is critical to `IHP.ControllerContext`, `IHP.FrameworkConfig`, every `option @MyType` call. The roadmap mentions `TypeApplications` as part of Phase 3.4 but does not call out that **value-level `@Type`** application requires parser+evaluator support, not just type-checker support.

### Surprise 4: `IncoherentInstances` in core dispatch (10 files)
IHP uses `IncoherentInstances` in `IHP.ModelSupport`, `IHP.Controller.Param`, `IHP.View.Form.*`, `IHP.Server` — the most central files. This means ihc's instance resolution must support "pick any matching instance" semantics, not just "fail on ambiguity." Without it, form field rendering and request parameter parsing crash. Not on the roadmap.

### Surprise 5: `GHC2021` default-language with package-level implications
ALL IHP packages use `default-language: GHC2021`. GHC2021 implicitly enables ~20 extensions including `GADTs`, `ScopedTypeVariables`, `MultiParamTypeClasses`, `TupleSections`, `StandaloneDeriving`, `ExistentialQuantification`, `NamedFieldPuns`, etc. This means ihc's cabal-aware loader needs to know what GHC2021 implies and apply the full set — not just the explicitly listed ones. The cabal loader (Phase 2.7) currently reads `default-extensions` from `.cabal` but it's unclear whether it correctly expands `default-language: GHC2021` to its implied extension set.

### Surprise 6: `ApplicativeDo` in `IHP.FetchPipelined` and generated code
`ihp-schema-compiler` generates `{-# LANGUAGE ApplicativeDo #-}` in every generated `*.hs` file for hasql row decoders. The generated code uses `do`-notation that GHC is expected to desugar as `<*>` rather than `>>=`. Without `ApplicativeDo` support in ihc's `do`-desugaring, every generated decoder silently reverts to monadic (sequential) desugaring — which changes semantics for the hasql pipeline. Not on the roadmap.

### Surprise 7: `PackageImports` in schema compiler
`ihp-schema-compiler/IHP/SchemaCompiler.hs:20` uses `import "interpolate" Data.String.Interpolate (i)`. The string `"interpolate"` is a package qualifier in the import statement. ihc's module loader does not support package-qualified imports (the `PackageImports` extension). Fortunately this is isolated to the schema compiler (one file), but if that file is interpreted it will fail to parse the import.

### Surprise 8: `DefaultSignatures` for opt-in instances
`IHP.View.Form.Select` uses:
```haskell
default selectLabel :: Show model => model -> Text
```
This lets types get a free default method implementation just by having a `Show` instance (without writing any `instance` body). Used in 3 places. ihc's class system does not support `DefaultSignatures`. Not on the roadmap.

### Surprise 9: `DuplicateRecordFields` + `DisambiguateRecordFields` as default extension
Multiple IHP packages enable `DuplicateRecordFields` as a cabal default-extension. This means a module can have two `data` types both with a `name :: Text` field. ihc currently has no mechanism to disambiguate record fields — all fields go into a flat namespace. This is a latent correctness bug for any non-trivial IHP module.

### Surprise 10: `typerep-map` (the `TMap` library) uses unsafe operations
`IHP.ControllerContext` and `IHP.FrameworkConfig` store typed values in a `TMap.TMap` — a type-indexed heterogeneous map. The `typerep-map` library internally uses `unsafeCoerce` to retrieve values by `TypeRep` key. To interpret this from source, ihc would need to implement `unsafeCoerce` as a host-Haskell coercion. More importantly, `TMap.lookup @Foo` requires `TypeApplications` at the call site AND `Typeable` for the `TypeRep` key. Without both, the context system is broken.

---

## Recommended Additions to the Roadmap

Ordered by how blocking each is for "run a real IHP app under ihc."

| Priority | Feature | What blocks without it | Difficulty |
|----------|---------|----------------------|------------|
| 1 | **ImplicitParams** (Phase 3.6) | The entire IHP context system — every controller, every view, every DB query | Deep: new calling convention; `let ?x = v` binding propagates through closures. Estimate: 2–4 weeks. |
| 2 | **`OverloadedLabels`** (Phase 3.5) | All `filterWhere`, `set`, `get`, `orderBy` calls | Medium: lexer + `fromLabel @"foo"` dispatch. Estimate: 1 week. |
| 3 | **`AllowAmbiguousTypes` + value-level `@Type` application** | `TypeMap.lookup @Foo`, `Proxy @name`, any `AllowAmbiguousTypes` function | Medium: expression-level `@Type` parsing and evaluation. Estimate: 1–2 weeks. |
| 4 | **`ScopedTypeVariables` + inner `forall`** | `TransactionRunner.runInTransaction`, any rank-2 function | Medium: type sig parser extension. Estimate: 1 week. |
| 5 | **`GHC2021` extension set expansion in cabal loader** | Silent wrong-behavior for all 20+ packages using `default-language: GHC2021` | Quick win: static table `GHC2021` → implied extensions. Estimate: 1–2 days. |
| 6 | **`FunctionalDependencies`** (`class C a b \| a -> b`) | `SetField`, `UpdateField` — model field update system | Medium: affects instance resolution. Estimate: 1–2 weeks. |
| 7 | **`IncoherentInstances`** | `IHP.Controller.Param`, `IHP.View.Form.*` — param parsing, form rendering | Medium: instance resolution policy. Estimate: 1 week. |
| 8 | **`StandaloneDeriving`** | Generated model types `deriving instance Show (Foo a)` | Quick win: parse + synthesize. Estimate: 3–5 days. |
| 9 | **`InstanceSigs`** | 13 files; parse errors without it | Quick win: parse and ignore. Estimate: 2–3 days. |
| 10 | **`ApplicativeDo` desugaring** | Generated hasql decoders; semantics change | Deep: dataflow analysis. Estimate: 1–2 weeks. |
| 11 | **`DuplicateRecordFields` / `DisambiguateRecordFields`** | Correctness bug for modules with shared field names | Medium: disambiguation during eval. Estimate: 1–2 weeks. |
| 12 | **`DefaultSignatures`** | 3 sites; form field opt-in instances | Medium: class parsing + instance elaboration. Estimate: 1 week. |
| 13 | **`PackageImports`** | One file in schema compiler | Quick win: ignore package qualifier. Estimate: 1 day. |
| 14 | **`ExistentialQuantification`** | `SomeView` in job dashboard | Medium: bundled with GADTs (Phase 2.9.5). |
| 15 | **`NamedFieldPuns`** | 6 files using `Foo { x }` pattern | Quick win: desugar to `Foo { x = x }`. Estimate: 1 day. |

---

## Quick-Win vs. Deep Rabbit Hole Assessment

**Quick wins (under 1 week each):**
- `GHC2021` extension expansion — a static lookup table in the cabal loader
- `PackageImports` — strip package qualifier during import parsing
- `InstanceSigs` — accept and ignore type signatures in instance blocks
- `NamedFieldPuns` — pattern desugaring (`Foo { x }` → `Foo { x = x }`)
- `StandaloneDeriving` — parse `deriving instance` as top-level, synthesize dictionary

**Deep rabbit holes:**
- `ImplicitParams` — requires propagating hidden extra arguments through the evaluator's closure model, similar in depth to a new calling convention. Every function with `?ctx` in its type needs to receive and pass through the implicit value. This interacts with closures, partial application, and multi-module loading.
- `ApplicativeDo` — requires dataflow analysis on `do`-blocks to detect independent binds and rewrite to applicative.
- `FunctionalDependencies` — requires a more sophisticated instance resolver that tracks functional dependencies to disambiguate multi-param class instances.

**The single most important missing feature for IHP compatibility is `ImplicitParams` (Phase 3.6).** Without it, you cannot call a single IHP controller action or render a single view. Every function in IHP's application layer has `?context` or `?modelContext` in its type.
