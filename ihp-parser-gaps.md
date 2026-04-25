# IHP Parser Gap Analysis

Probe date: 2026-04-16. Probe tool: `ihc-parse-probe` (new `exe:ihc-parse-probe` in `ihc.cabal`).
Sample: **50 IHP source files** from `IHP/`, `IHP/Controller/`, `IHP/HSX/`, `IHP/ServerSideComponent/`, integration tests.
Totals: **455 top-level bindings** scanned, **360 parse OK (79%)**, **95 parse errors (21%)**, **20 of 50 files have at least one error**.

---

## Error Buckets

| Error bucket | Occurrences | Files affected | Example file:line:col | Root cause | Est. effort |
|---|---|---|---|---|---|
| `expected identifier after let; saw TkImplicitRef` | 22 | 10 | `IHP/ModelSupport.hs:3:9` | `let ?foo = expr` implicit-parameter let-binding: parser's `singleBind` requires `TkIdent` but sees `TkImplicitRef`. The `parseImplicitLet` path exists but is only reached at the let-entry check; the singleBind sub-path rejects `TkImplicitRef` on the LHS. Also: `let !x = expr` (bang-strict let) hits same gate. | **1 day** — wire `TkImplicitRef` and `TkBang` into `singleBind` & the layout-let path |
| `expected = in binding; saw TkDColon` | 16 | 9 | `IHP/ModelSupport.hs:2:24` | Type signatures in `where`-clause bodies (`didChangeField :: Bool`) are scanned as bindings by `scanAllTopLevelNames`/`findBinding`; the `parseBindingsIn` path then tries to parse `identifier :: Type` as `identifier = expr` and fails at `::`. Need to skip `name :: type` declarations inside where-blocks. | **1-2 days** — add type-sig skip in `parseBindingsIn`; teach where-parser to ignore `name :: T` | 
| `tuple/section syntax; saw TkDColon` | 15 | 6 | `IHP/RouterSupport.hs:1:22` | Pattern tuples containing type annotations `(e :: SomeException)` or record patterns with `::` inside parens. Parser's paren-pattern loop expects `,` or `)` but hits `::` type annotation. Needs `(pat :: Type)` inside pattern context. | **2 days** — accept `:: Type` inside paren-pattern and paren-expression; essentially `(e :: T)` desugaring | 
| `expected \`of\` in case-expression; saw TkDot` | 11 | 6 | `IHP/ModelSupport.hs:1:21` | `case ?implicitParam.field of` — parser parses `?implicitParam` as the scrutinee atom, then sees `.field` and thinks the case is complete (expecting `of`). The dot-chain `?x.y.z` must extend the `EImplicitRef` atom via record-dot postfix in `parseAtom`/`parsePostfix`. | **1 day** — extend postfix dot-chain to apply after `TkImplicitRef` atoms (same as after `TkIdent`/`TkConId`) |
| `expected \`in\` in let-binding; saw TkEq` | 7 | 5 | `IHP/ModelSupport.hs:4:20` | Multi-binding layout `let` without braces: `let\n  x = 1\n  y = 2\nin ...`. Parser's `singleBind` consumes one binding and immediately demands `in`, but the second binding appears before `in` at the same indent level. Needs a layout-let loop. | **2 days** — implement layout-sensitive multi-bind let (collect all same-indent bindings before `in`) |
| `expected \`->\` in case alternative; saw TkDot` | 5 | 4 | `IHP/ErrorController.hs:3:20` | Pattern `SomeException e` or qualified constructor `ErrorController.RouterException e` in case alt: parser is inside pattern parse loop and sees `.` (qualified module separator) where it expects `->`. Needs qualified-constructor patterns `Module.Ctor pat*`. | **2 days** — handle `TkConId.TkConId...` in pattern context (qualified constructor) |
| `expected \`->\` in case alternative; saw TkBar` | 5 | 4 | `IHP/HSX/Parser.hs:7:13` | Multi-way case guards `case x of { p \| g -> e; ... }`: the `\|` after a pattern is a guard, not a separator. Parser's case-alt loop treats `\|` as unexpected when it appears before `->`. Needs guard support inside case alts. | **1-2 days** — already have guards in top-level RHS; thread them into case alt parsing |
| `expected ) or , in paren-expr; saw TkLBrace` | 5 | 4 | `IHP/AutoRefresh.hs:3:44` | Record update syntax `record { field = val }` when used inside a lambda arg `(\s -> s { field = val })`: the record-update `{ ... }` after an expression is not parsed. Needs `expr { field = val, ... }` postfix record-update. | **2-3 days** — add postfix record-update in `parsePostfix` |
| `expected = in let-binding; saw TkDColon` | 8 | 5 | `IHP/ModelSupport.hs:7:16` | Type signature inside a layout-let block (`let autoRefreshWSParser :: Parser Application`). The let-binding parser sees a type sig and tries to parse it as `name = expr`, failing at `::`. Same fix as the where-block type-sig issue. | **(shared with #2 above)** |
| `expected pattern or \`->\` in lambda; saw TkLBrace` | 1 | 1 | `IHP/AutoRefresh.hs:4:38` | Lambda with record-destructure pattern `\AutoRefreshSession { id } -> ...`: named-field pattern `Con { field }` in lambda param. Parser's lambda param loop doesn't handle `TkConId` followed by `{`. | **1 day** — add `Con { fields }` record pattern in `parseSubPat` |
| `expected \`=\` after guard; saw TkLArrow \| TkComma` | 2 | 2 | `IHP/RouterSupport.hs:1:13` | Guards with let-bindings in pattern context (`\| Just Refl <- eqT @d @T = ...`): `<-` is a pattern-guard bind. Parser sees `<-` where it expects `=` in a guard. TypeablePat syntax. | **2 days** — pattern guards (`g <- expr` in guard position) |
| `expected , or ] in list pattern; saw TkIdent` | 1 | 1 | `IHP/HSX/Parser.hs:1:11` | `[TextNode text]` in LHS pattern: constructor applied to variable inside list pattern. Parser treats list patterns as containing only literals/constructors, not `Con var` applications. | **1 day** — allow full pattern in list-pattern elements |
| `clauses have differing arities` | 1 | 1 | `IHP/ServerSideComponent/HtmlDiff.hs:0:0` | Multi-clause function where some clauses have more arguments than others due to guard/where interaction confusing arity counting. | **< 1 day** — diagnostic only, or align arity check |
| `unexpected token; saw TkWhere` | 1 | 1 | `IHP/HSX/Parser.hs:6:5` | `where` inside a `do`-block binding (function defined in a do-bind statement with its own `where`). Parser's `do`-block doesn't allow `where` on individual do-statements. | **1 day** — allow where-clause after a do-statement |

---

## Summary (top 5 gaps by IHP file coverage)

**Files clean: 19/50 (38%). Files broken: 20/50. Parse errors: 95 on 455 bindings.**

### 1. Implicit-parameter expressions without `.` postfix (11 errors, 6 files)
`case ?modelContext.transactionRunner of` — the `?x.field` chain doesn't extend past the implicit-param atom. Also fires as `let ?x = …` in singleBind. **Fix: extend `parsePostfix`/`parseAtom` to dot-chain after `TkImplicitRef`.** Estimated: **1 day**. Unblocks: `ModelSupport`, `ControllerSupport`, `RouterSupport`, `FileUpload`, `Controller/*`.

### 2. Type signatures in `where`-blocks and layout-`let` (24 errors, 12 files — combined buckets 2+8)
`where { name :: T }` and `let { name :: T; name = expr }` both trip the binding parser. **Fix: teach `parseBindingsIn` and the layout-let loop to skip `name :: T` declarations.** Estimated: **1-2 days** for the skip; another **2 days** for multi-binding layout-`let`. Together this unblocks: `ModelSupport`, `RouterSupport`, `AutoRefresh`, `HSX/Parser`, `HtmlDiff`, `ControllerSupport`, `Redirect`, `Render`.

### 3. Record-update postfix `expr { field = val }` (5+ errors, 4 files)
`\s -> s { sessions = … }` — record-update after an arbitrary expression. **Fix: add `{ name = expr, … }` postfix in `parsePostfix` (after `parsePrimary`).** Estimated: **2-3 days** (record update is a meaningful surface area). Unblocks: `AutoRefresh`, `HtmlDiff`, `ErrorController` (partial).

### 4. `(pat :: Type)` in pattern context + qualified constructors in patterns (16 errors, 9 files — combined buckets 3+5)
`(e :: SomeException)` in case patterns; `ErrorController.RouterException e` qualified constructor. **Fix: skip `:: Type` annotations in paren-patterns; parse `Con.Con` as qualified constructor in patterns.** Estimated: **2 days**. Unblocks: `RouterSupport`, `ErrorController`, `Sessions`, `HtmlDiff`.

### 5. Multi-alternative case guards `| guard -> body` and pattern guards `| p <- expr` (7 errors, 6 files)
Already partially present for top-level RHS guards. **Fix: thread guard support into case-alt parsing; add `p <- expr` pattern-guard form.** Estimated: **1-2 days**. Unblocks: `RouterSupport` (`renderFieldForUrl`), `HSX/Parser`.

---

## Honest size assessment

| Fix | Effort | Files directly unblocked |
|---|---|---|
| `?x.field` dot-chain postfix | **1 day** | 6 |
| Type-sig skip in where/let-blocks | **1-2 days** | 8+ |
| Multi-binding layout-`let` | **2 days** | 5+ |
| `(pat :: T)` + qualified constructor patterns | **2 days** | 6 |
| Case-alt guards + pattern guards | **1-2 days** | 6 |
| Record-update postfix `expr { f=v }` | **2-3 days** | 4 |
| Record-destructure lambda param `\Con { f } ->` | **1 day** | 2 |

**The first two items (`?x.field` + type-sig-skip) are 1-2 day slices each and together would clear ~15 of the 20 broken files.** The remaining items (record-update, qualified constructors, multi-bind let) are individually 2-3 day slices but collectively require ~1 week. None are multi-week undertakings given the existing parser infrastructure.

---

## HSX rendering

Rendering `[hsx|<h1>Hello world</h1>|]` end-to-end (quasi-quote → evaluated → HTML string) exposes a distinct set of blockers that do **not** overlap with the parser buckets above. These are the concrete gaps, in the order they surface during a first-run attempt. Sentinel test: `examples/hsx_hello/Main.hs` (see also — shipped by a parallel unit).

### 1. QuasiQuote parser emits a placeholder, not a call

`src/IHC/Parser.hs:2867–2880` (the `TkQQOpen qqName` branch): the body bytes are scanned opaquely via `skipQQBody`, and the AST node emitted is `EApp (EVar "error") (stringToConsList "unexpanded QuasiQuoter [name|…|]")`. Any HSX use site therefore evaluates to a runtime `error` — the QQ never reaches the evaluator as a real expression. Replacing this with proper dispatch is the first mandatory step. (A parallel unit in this batch is working on that — cross-reference it if landed, but this catalog entry stands alone.)

### 2. No QuasiQuoter registry / dispatch

Even with the parser fix, nothing in the evaluator maps the captured `qqName` (e.g. `"hsx"`) back to the `IHP.HSX.QQ.hsx` `QuasiQuoter` value and calls its `quoteExp :: String -> Q Exp`. `src/IHC/TH.hs` (889 lines) holds all the existing TH machinery — `thExpToExpr`, `expandSplicesInExpr`, `thDecsToBindings` — but has no `quoteExp`/`QuasiQuoter` support (`Grep` for `quoteExp|QuasiQuoter|qqName` in `TH.hs` returns zero matches). The dispatch layer needs to: (a) parse the body as whatever syntax the QQ accepts, (b) drive the QQ's `quoteExp` through the Q monad, (c) hand the resulting `TH.Exp` to `thExpToExpr` to materialize an `IHC.AST.Expr`, (d) splice it in place of the QQ node. Estimated shape: new exports in `IHC.TH` plus a hook in the scheduler's `expandSplicesInModule` pass.

### 3. Package sources must be cached

`ihp-hsx`, `blaze-html`, `blaze-markup`, `blaze-builder`, `megaparsec`, `string-conversions` — none of these have bespoke host-side shims (and per the project policy in `CLAUDE.md` "Builtin modules: minimum surface only" they **must not**). All six must be materialized under `~/.cache/ihc/sources/<pkg>-<version>/` so `Scan`/`Source` can pick them up. A parallel unit in this batch ships the fetch script — until it runs, source-interpretation of the HSX pipeline cannot start.

### 4. TH / QQ language gaps (high level)

HSX's `quoteExp` is a non-trivial use of the TH API: it calls `parseHsx`, builds `Exp` trees with `AppE`, `VarE`, `ConE`, `LitE`, `ListE`, `TupE`, uses `newName`, and threads everything through the `Q` monad. `IHC.TH` today only supports the Lift-splice subset (see the module header comment at `src/IHC/TH.hs:1–21`: "NOT in scope: `[| |]` quotation, reify, Q IO, declaration/type splices"). The full catalog of TH surface HSX requires is documented in `docs/HSX-TH-NEEDS.md` (see also — shipped by a parallel unit). Key items expected there: `Q` monad beyond Lift, `newName` / name freshening, `QuasiQuoter` record type, nested bracket support.

### 5. Class dispatch

Runtime rendering exercises at minimum: `Text.Blaze.ToMarkup` (instances for `String`, `Text`, `Html`, `Int`, …), `Text.Blaze.ToValue` (for attribute values), and the internal `Text.Blaze.Internal.Markup` monoid machinery. Class-dispatch in `src/IHC/Eval.hs` (`VClassMethod` at lines 410, 946, 1024, 1033 — constructor, pattern-match guard, and the two `apply`/`applyIP` branches) already handles polymorphic dispatch via the `tags` list, but every new instance from blaze must be registered via the existing class registry. Expect "no instance for ToMarkup <T>" errors until each blaze instance is source-loaded.

### 6. Renderer primops

`Text.Blaze.Html.Renderer.String.renderHtml` lives in blaze-html source and, in principle, needs no host primop — it's ordinary Haskell over the `Markup` tree. In practice it delegates to `Data.ByteString.Builder` primitives (`Builder`, `toLazyByteString`, UTF-8 encoders) whose primop coverage in IHC is **unverified**; first-run will likely reveal missing `GHC.Prim`-backed builder ops. Mark as **unverified — needs first-run debugging** before committing to a fix size.

### 7. Sentinel test

`examples/hsx_hello/Main.hs` (see also — shipped by a parallel unit) drives `[hsx|<h1>Hello world</h1>|]` through `renderHtml` and prints the result. Green = full pipeline working; red points at whichever of 1–6 above fails first.

### Ordering

1, 2, and 3 are hard prerequisites — nothing downstream can be exercised without them. 4 depends on 2. 5 and 6 are first-runnable only after 1–4 land. None of the HSX blockers overlap with the parser buckets above.
