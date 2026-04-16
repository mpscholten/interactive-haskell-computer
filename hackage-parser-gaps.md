# Hackage Parser Gap Analysis

Probe date: 2026-04-16. Probe tool: `ihc-parse-probe` (exe in `ihc.cabal`).
Packages: hasql-1.10.3 · servant-0.20.3.0 · lens-5.3.6 · conduit-1.3.6.1 · hasql-pool-1.4.2 · servant-server-0.20.3.0.

---

## Per-package results

### 1. hasql-1.10.3

**Files probed: 45 · Errors: 30 · OK: 341 · Pass rate: 92% bindings, 58% files (26/45 clean)**

| Bucket | Count | Files | Example file:line:col | Root cause | Effort |
|---|---|---|---|---|---|
| `tuple/section syntax` (saw `TkDot`) | 14 | 6 | `Hasql/Pipeline.hs:1:18` | Qualified field access as function arg: `f (Rec.field x)` — the `.` inside a paren arg is record-dot syntax, but the parser's paren-expression loop hits `TkDot` expecting `,` or `)`. Pattern `(Roundtrip.toSerialIO x)` same root. This is **record-dot / qualified-dot in paren-expression/pattern context**. | **2 days** — extend paren-expr and paren-pattern to follow `.` chains |
| `lambda/case arrow` (saw `TkDot`) | 11 | 8 | `Hasql/Codecs/Encoders/Composite.hs:2:16` | `case x.field of` — record-dot in the scrutinee of a case. The parser consumes `x` as the scrutinee atom, then hits `.field` expecting `of`. This is the IHP `?x.field` postfix-dot gap generalised to normal fields. | **1 day** — extend `parsePostfix` to follow `TkDot` after any atom (already done for `TkImplicitRef` in IHP punchlist; generalise) |
| `expected = or | at RHS start` (saw `TkDColon`) | 2 | 2 | `Hasql/Connection.hs:2:30` | Type signature in `where` block scanned as a binding; `name :: Type` hits `::` where `=` expected. Same as IHP bucket 2. | **1-2 days** — shared with IHP fix |
| `expected identifier in binding` (saw `TkConId`) | 2 | 2 | `Hasql/Comms/Roundtrip.hs:2:5` | Function clause `Roundtrip { .. } = ...` — record-pattern constructor on the LHS. The binding scanner picks up constructor name as the function identifier, then the second clause `Roundtrip { }` is parsed as a new binding starting with a constructor. | **1-2 days** — handle `ConId { fields }` record pattern as LHS clause of an existing binding |
| `let-in expression` (saw `TkEq`) | 1 | 1 | `Hasql/Codecs/Encoders/Array.hs:4:20` | Multi-binding layout `let`. Shared with IHP bucket 5. | **2 days** — shared with IHP fix |

---

### 2. servant-0.20.3.0

**Files probed: 41 · Errors: 4 · OK: 64 · Pass rate: 94% bindings, 29% files (12/41 clean, 25 files have 0 bindings)**

| Bucket | Count | Files | Example file:line:col | Root cause | Effort |
|---|---|---|---|---|---|
| `expected = or | at RHS start` (saw `TkDColon`) | 4 | 3 | `Servant/Types/SourceT.hs:4:8` | Type signature inside `where` block. `readFile :: FilePath -> ...` inside a `where` block is scanned as a binding and then fails at `::`. Same as IHP bucket 2 and hasql bucket 3. | **1-2 days** — shared fix |

Notes: Servant is nearly clean (94% binding pass rate). The type-level combinator definitions (`:>`, `:<|>`, DataKinds instances) are all type-level and live in data/class/instance declarations rather than value bindings — these are not probed by the current tool which only probes RHS expressions.

---

### 3. lens-5.3.6

**Files probed: 35 (Core/Lens subtree) · Errors: 30 · OK: 426 · Pass rate: 93% bindings, 57% files (15/35 clean)**

| Bucket | Count | Files | Example file:line:col | Root cause | Effort |
|---|---|---|---|---|---|
| `expected = or | at RHS start` (saw `TkDColon`) | 5 | 3 | `Control/Lens/Internal/FieldTH.hs:2:14` | Type signature in `where` block. Shared across all packages. | **1-2 days** — shared fix |
| `tuple/section syntax` (saw `TkLBrace`) | 5 | 2 | `Control/Lens/Internal/PrismTH.hs:1:17` | Record update in expression: `(nconTypes con)` is parsed, then `{ types = ... }` follows as postfix record-update. Parser sees `{` where it expects `,` or `)`. This is **record-update postfix** (also IHP bucket 7). | **2-3 days** — shared with IHP fix |
| `expected = after guard` (saw `TkLArrow`) | 4 | 3 | `Control/Lens/Internal/FieldTH.hs:1:10` | Pattern guards in top-level bindings: `f x \| Just y <- expr = ...`. The `<-` in a guard position is a pattern-guard bind. Parser sees `<-` where it expects `=` after a guard. | **2 days** — shared with IHP bucket 10 (pattern guards) |
| `expected identifier in binding` (saw `TkAs`) | 4 | 3 | `Control/Lens/Fold.hs:1:2` | Binding `repeated f a = as where as = ...` — lexer tokenises `as` as `TkAs` (the import `as` keyword) even when used as a plain identifier. **`as` is a soft keyword in Haskell** and must be allowed as an ordinary identifier. | **< 1 day** — demote `as` (and likely `qualified`, `hiding`) from hard to soft keywords in the lexer |
| `lambda/case arrow` (saw `TkBar`) | 3 | 2 | `Control/Lens/Internal/FieldTH.hs:3:33` | Multi-way case guards: `case x of { p \| g -> e }`. The guard `\|` inside a case alt is not parsed. Shared with IHP bucket 6. | **1-2 days** — shared fix |
| `expected = in let-binding; saw TkBar` | 3 | 2 | `Control/Lens/Internal/FieldTH.hs:10:12` | Layout `let` with guarded bindings: `let f x \| guard = e1 \| otherwise = e2`. The `\|` guard inside a `let` binding is not handled. | **1-2 days** — extend let-binding parser to accept guards (same fix as case-alt guards) |
| `let-in expression` (saw `TkEq`) | 2 | 1 | `Control/Lens/Fold.hs:4:10` | Multi-binding layout `let`. Shared. | **2 days** |
| `case-of expression` (saw `TkSymOp "^?"`) | 1 | 1 | `Control/Lens/Fold.hs:1:12` | `filteredBy l = \f s -> case s ^? l of ...` — operator `^?` in scrutinee position. The parser's scrutinee parse doesn't extend to infix operator expressions; it stops at `^?` because it's not a primary-expr token. **Operator-application in case scrutinee** needs to allow full expression before `of`. | **1-2 days** — allow full infixExpr as case scrutinee (not just atom) |
| `differing arities` | 1 | 1 | `Control/Lens/Internal/Level.hs:0:0` | `lappend` has two separate clause groups with different arity due to guard structure. Shared diagnostic. | **< 1 day** |
| `expected pattern; saw TkSymOp ":<"` | 1 | 1 | `Control/Lens/Cons.hs:1:2` | `pattern (:<)` — **`PatternSynonyms`** declaration. The probe scans `pattern` as a binding name, then tries to parse `(:<) = ...` as an expression, failing at the operator in paren. Novel: IHP doesn't use `PatternSynonyms` declarations. | **2-3 days** — parse `pattern` declaration in the binding scanner |
| `expected = in pattern let-binding; saw TkIdent` | 1 | 1 | `Control/Lens/Profunctor.hs:3:24` | `let Context f a = l sell s in ...` — destructor pattern `let Con f a = expr`. The layout-let parser only handles `let var = expr`; a constructor pattern on the LHS is not supported. | **1 day** — allow constructor patterns in `let`-binding LHS |
| `expected identifier in backtick section; saw TkConId "Set"` | 1+1 | 1+1 | `Control/Lens/Internal/TH.hs:2:38` | **Qualified backtick operator** `` `Set.notMember` ``. The backtick-section parser expects an unqualified identifier but sees `TkConId "Set"` followed by `.`. | **< 1 day** — allow `ConId.ident` inside backtick sections |

---

### 4. conduit-1.3.6.1

**Files probed: 14 (all src) · Errors: 44 · OK: 391 · Pass rate: 69% bindings, 43% files (3/14 clean)**

**Most broken package** in the probe.

| Bucket | Count | Files | Example file:line:col | Root cause | Effort |
|---|---|---|---|---|---|
| `let-in expression` (saw `TkEq` or `TkWhere`) | 20 | 3 | `Conduit/Internal/Conduit.hs:3:25` | Multi-binding layout `let`. Many conduit functions use `let { loop = ...; go = ... } in ...` patterns and also `let { f ... = ... where localWhere }` where an inner function in the let-block has its own `where` clause. Two sub-cases: (a) multiple bindings before `in`; (b) `let` binding with `where` sub-clause. | **2 days** — multi-binding let (shared); **+1 day** for where-in-let |
| `tuple/section syntax` (saw `TkDot` or `TkUnderscore`) | 7 | 3 | `Conduit/Internal/Conduit.hs:3:19` | Record-dot in paren expression `(x.field)`. `TkUnderscore` variant comes from `let (_, !t) = ...` — tuple destructure with wildcard in let. | **2 days** — paren-dot (shared); **< 1 day** for wildcard in let-pattern |
| `unexpected token` (saw `TkUnderscore` or `TkBang`) | 5 | 3 | `Conduit/Internal/Conduit.hs:2:6` | (a) `let (_, u) <- loop ...` — wildcard `_` in tuple destructure in a `do`-bind; (b) `!seed' <- f seed x` — bang-strict do-bind `!x <- expr`. Two gaps: wildcard in do-bind pattern; strict do-bind. | **1 day** — allow `_` and `!pat` in do-bind LHS patterns |
| `lambda/case arrow` (saw `TkBar`) | 4 | 3 | `Conduit/List.hs:8:17` | Case-alt guards (`case x of { p \| g -> e }`). Shared. | **1-2 days** |
| `expected = or | at RHS start` (saw `TkDColon`) | 2 | 2 | `Conduit/Internal/Conduit.hs:2:10` | Type sig in where block. Shared. | **1-2 days** |
| `expected identifier in binding` (saw `TkLParen`) | 2 | 2 | `Conduit/Lift.hs:1:15` | `let (accH, accT) = ...` — tuple-destructure in `let`. The binding scanner enters `singleBind`, expects identifier, sees `(`. Also covers `let (_, res) <- ...`. | **1 day** — allow paren-tuple pattern in let/do-bind scanner |
| `expected = in let-binding; saw TkBar` | 2 | 1 | `Conduit/Combinators.hs:3:14` | Guarded let-binding. Shared with lens bucket 6. | **1-2 days** |
| `expected identifier in backtick section; saw TkConId` | 1 | 1 | `Conduit/Lift.hs:1:15` | `` `R.runReaderT` `` — qualified backtick. Shared with lens. | **< 1 day** |
| `expected = in pattern let-binding; saw TkUnderscore` | 1 | 1 | `Conduit/Internal/Conduit.hs:2:14` | `let _ = ...` — wildcard binding in let. Parser expects identifier, sees `_`. | **< 1 day** — allow `_` as let-LHS pattern |

---

### 5. hasql-pool-1.4.2

**Files probed: 8 · Errors: 14 · OK: 15 · Pass rate: 52% bindings, 38% files (3/8 clean)**

| Bucket | Count | Files | Example file:line:col | Root cause | Effort |
|---|---|---|---|---|---|
| `tuple/section syntax` (saw `TkLBrace`) | 9 | 2 | `Hasql/Pool/Config/Setting.hs:2:30` | Record update in lambda: `Setting (\config -> config { Config.initSession = x })`. The `{ ... }` after an expression in a lambda body hits `TkLBrace` where the paren-expr loop expects `,` or `)`. This is **record-update postfix** (IHP bucket 7 + lens bucket 2). | **2-3 days** — shared fix |
| `lambda/case arrow` (saw `TkDot`) | 4 | 2 | `Hasql/Pool/SessionErrorDestructors.hs:2:9` | `case x.field of` — record-dot in case scrutinee. Shared with hasql bucket 2. | **1 day** — shared fix |
| `expected identifier in binding` (saw `TkLParen`) | 1 | 1 | `Hasql/Pool/exposed/Pool.hs:12:11` | `let (a, b) = ...` — tuple-destructure in let. Shared with conduit bucket 6. | **1 day** — shared fix |

---

### 6. servant-server-0.20.3.0

**Files probed: 19 · Errors: 11 · OK: 101 · Pass rate: 90% bindings, 47% files (9/19 clean)**

| Bucket | Count | Files | Example file:line:col | Root cause | Effort |
|---|---|---|---|---|---|
| `expected = or | at RHS start` (saw `TkDColon`) | 4 | 3 | `Server/Internal/Router.hs:2:16` | Type sig in where block. Shared. | **1-2 days** |
| `expected identifier in binding` (saw `TkLParen`) | 2 | 2 | `Server/Internal/BasicAuth.hs:3:9` + `Server/Internal.hs:7:20` | `let (a, b) = ...` and `let (headers, body) = ...` — tuple-destructure in let. Shared. | **1 day** |
| `expected = in pattern let-binding; saw TkIdent` | 2 | 2 | `Server/Internal/Context.hs:2:20` | `let Context f a = ...` — constructor pattern in let LHS. Shared with lens bucket 10. | **1 day** |
| `lambda/case arrow` (saw `TkBar`) | 1 | 1 | `Server/Internal/Router.hs:8:22` | Case-alt guards. Shared. | **1-2 days** |
| `let-in expression` (saw `TkEq`) | 1 | 1 | `Server/Internal.hs:3:20` | Multi-binding layout let. Shared. | **2 days** |
| `tuple/section syntax` (saw `TkRBracket`) | 1 | 1 | `Server/Server.hs:1:46` | `hoistServer p = hoistServerWithContext p (Proxy :: Proxy '[])`  — `'[]` promoted empty list type in expression context. Parser sees `'[` then `]` and hits `TkRBracket` where `,` or another element is expected. **Promoted list type syntax `'[...]`** in expression position. | **1 day** — parse `'[...]` as a promoted-list expression |

---

## Cross-cutting summary

Buckets appearing in **3 or more packages** (highest ROI):

| Bucket | Packages (count) | Total errors | Cumulative effort |
|---|---|---|---|
| Type-sig in where/let block (`saw TkDColon`) | hasql, servant, lens, conduit, servant-server (**5/6**) | 17 | **1-2 days** |
| Multi-binding layout `let` (`saw TkEq` / `TkWhere` / `TkSemi`) | hasql, lens, conduit, servant-server (**4/6**) | 24 | **2 days** |
| Record-dot in paren/case/postfix (`saw TkDot`) | hasql, hasql-pool, conduit (**3/6**) | 29 | **1 day** (generalise IHP fix) |
| Record-update postfix `expr { f=v }` (`saw TkLBrace`) | lens, hasql-pool, servant-server (**3/6**) | 15 | **2-3 days** |
| Case-alt guards (`saw TkBar` in case alt) | lens, conduit, servant-server (**3/6**) | 8 | **1-2 days** |
| Tuple-destructure in let/do `(a, b) = ...` (`saw TkLParen`) | conduit, hasql-pool, servant-server (**3/6**) | 5 | **1 day** |

---

## Novel buckets (new vs IHP, seen in 1-2 packages)

| Bucket | Package | Errors | Root cause | Effort |
|---|---|---|---|---|
| `as` soft-keyword as identifier (`TkAs`) | lens | 4 | Lexer hard-codes `as` as a keyword; Haskell spec says it is a soft keyword and must be usable as a variable name. | **< 1 day** |
| Qualified backtick operator `` `Mod.fn` `` | lens, conduit (2) | 2 | Backtick parser only accepts unqualified identifiers. | **< 1 day** |
| Operator in case scrutinee (`case x ^? l of`) | lens | 1 | Scrutinee parser stops at first non-primary token; should parse full infixExpr. | **1-2 days** |
| `PatternSynonyms` declaration (`pattern (:<)`) | lens | 1 | `pattern` keyword not recognised as a declaration type; probe scans it as a binding. | **2-3 days** |
| Constructor pattern in let LHS (`let Con f a = ...`) | lens, servant-server (2) | 3 | Let-parser only handles identifier on LHS, not constructor-patterns. | **1 day** |
| Promoted list syntax `'[...]` in expr | servant-server | 1 | `'[` opening is not parsed as a promoted-list expression/type; just hits `'` as a tick operator. | **1 day** |
| Bang-strict do-bind `!x <- expr` | conduit | 2 | Do-bind LHS parser only allows plain patterns, not bang-patterns. | **< 1 day** |
| Wildcard `_` in do-bind / let LHS | conduit | 2 | `_` not accepted as a pattern in do-bind or let-LHS. | **< 1 day** |
| `where`-clause inside a `let`-block binding | conduit | 2 | Local function defined in a `let` block with its own `where`; parser doesn't allow `where` on a let-binding. | **1-2 days** |

---

## Overall pass rates

| Package | Files probed | Files clean | Binding pass rate |
|---|---|---|---|
| hasql-1.10.3 | 45 | 26 (58%) | 341/372 = **92%** |
| servant-0.20.3.0 | 41 | 12 (29%) | 64/68 = **94%** |
| lens-5.3.6 | 35 | 15 (43%) | 426/456 = **93%** |
| conduit-1.3.6.1 | 14 | 3 (21%) | 391/569 = **69%** |
| hasql-pool-1.4.2 | 8 | 3 (38%) | 15/29 = **52%** |
| servant-server-0.20.3.0 | 19 | 9 (47%) | 101/112 = **90%** |

**Most broken package: conduit** (69% binding pass rate, only 3/14 files fully clean). The dominant cause is multi-binding layout `let` — conduit uses it extensively for recursive `loop`/`go` helpers.

**Most surprising idiom: `as` as a variable name** (lens `Fold.hs`: `repeated f a = as where as = f a .> as`). The `as` soft-keyword is lexed as `TkAs`, silently breaking any Haskell code that uses `as` as an ordinary identifier. This is a low-effort fix with broad correctness impact.

---

## Prioritised punchlist (new-vs-IHP items, by ROI)

Fixing the 6 IHP-shared gaps (already in `ihp-parser-gaps.md`) would clear most errors. The new items ranked by impact:

| Priority | Fix | Effort | New errors cleared | Packages |
|---|---|---|---|---|
| 1 | Record-dot postfix after any atom (generalise from `?x.y` to `x.y`) | **1 day** | ~29 | hasql, hasql-pool, conduit |
| 2 | Tuple-destructure in let/do-bind LHS `(a, b) = ...` | **1 day** | ~7 | conduit, hasql-pool, servant-server |
| 3 | Constructor pattern in let LHS `let Con f a = ...` | **1 day** | ~3 | lens, servant-server |
| 4 | `as` demoted to soft keyword in lexer | **< 1 day** | ~4 | lens |
| 5 | Qualified backtick operators `` `Mod.f` `` | **< 1 day** | ~2 | lens, conduit |
| 6 | Wildcard `_` and bang `!x` in do-bind/let-LHS | **< 1 day** | ~4 | conduit |
| 7 | Promoted list `'[...]` in expression | **1 day** | ~1 | servant-server |
| 8 | Operator in case scrutinee (full infixExpr before `of`) | **1-2 days** | ~1 | lens |
| 9 | `PatternSynonyms` declaration form | **2-3 days** | ~1 | lens |
| 10 | `where`-clause inside a let-block binding | **1-2 days** | ~2 | conduit |

Items 1–6 are **< 1 day each** and together would clear ~49 of the 133 errors across these 6 packages that are NOT already covered by the IHP punchlist fixes.
