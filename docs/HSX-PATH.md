# HSX hello-world: architecture & path to rendered HTML

Status: **foundation phase**. End-to-end route from `[hsx|<h1>Hello
world</h1>|]` source text to a rendered HTML `String` under IHC. Map
for follow-up sessions.

Sibling slices this batch: Unit 1 = cache-priming script, Unit 2 =
smoke fixture, Unit 3 = parser `EQuasiQuote` wiring, Units 5/6/7 =
evaluator / renderer / class-dispatch follow-ups. This is Unit 4.

## 1. Goal

Render a single HSX QuasiQuoter in the interpreter:

```haskell
import IHP.HSX.QQ (hsx)
import Text.Blaze.Html.Renderer.String (renderHtml)

main :: IO ()
main = putStrLn (renderHtml [hsx|<h1>Hello world</h1>|])
```

Expected stdout: `<h1>Hello world</h1>`. Everything must flow through
the interpreter — parse, QQ expansion, TH AST decode, evaluation,
blaze rendering. No host short-cuts.

---

## 2. Constraint: no shims

The project's `CLAUDE.md` (quoted verbatim):

> Keep the set of host-backed / "builtin" modules to an absolute
> minimum. The only modules that may be host-backed are those with **no
> Haskell source at all** … Anything with real `.hs` source in `base`
> (or in any Hackage package) must be interpreted from that source, not
> short-circuited via a builtin shim.
>
> 4. **No shims for ordinary Hackage libraries.** Do NOT host-shim
>    `tasty`, `optparse-applicative`, `containers`, `Data.Text`,
>    `aeson`, etc. — even if it's faster to ship.
>
> If interpreting a module from source reveals a missing language
> extension, primop, or class-dispatch case, the correct response is to
> **implement the missing feature**, not to add another shim.

So: `IHP.HSX.QQ`, `IHP.HSX.Parser`, `Text.Blaze.*`, and every transitive
dep must be loaded from their tarball in `~/.cache/ihc/sources/`. All
source is already on disk (see §3); the work is feeding it through the
existing loader and filling evaluator gaps that emerge.

The `isBuiltinBackedModule` whitelist (`src/IHC/Scheduler.hs:4321-4344`)
today contains only source-less `GHC.*` modules. No HSX-related module
may be added to it.

---

## 3. Dependency tree

Transitive deps pulled in by `ihp-hsx` that are relevant to an HSX
rendering call. Cache check: `ls ~/.cache/ihc/sources/`.

| Package            | Cached version                    | Role |
|--------------------|-----------------------------------|------|
| `ihp-hsx`          | `ihp-hsx-1.5.0`                   | `IHP.HSX.QQ` (the `hsx` QuasiQuoter), `IHP.HSX.Parser` (megaparsec-based HSX tokenizer), `IHP.HSX.ToHtml`, `IHP.HSX.Attribute`, `IHP.HSX.ConvertibleStrings`, `IHP.HSX.HsExpToTH`, `IHP.HSX.HaskellParser`. |
| `template-haskell` | `template-haskell-2.22.0.0`       | `Language.Haskell.TH`, `Language.Haskell.TH.Syntax`, `Language.Haskell.TH.Quote` — `QuasiQuoter { quoteExp, quotePat, quoteDec, quoteType }`. |
| `text`             | `text-2.1.1` + `text-2.1.4`       | `Data.Text`, `Data.Text.Encoding`. |
| `bytestring`       | `bytestring-0.12.2.0`             | Blaze builder output. |
| `containers`       | `containers-0.6.7`                | `Data.Set`, `Data.Map` — HSX uses `Set.empty` for settings. |
| `unordered-containers` | `unordered-containers-0.2.20` | `Data.HashMap.Strict` — HSX uses it for attribute tables. |
| `megaparsec`       | `megaparsec-9.7.0`                | HSX's tag/attribute parser lives on top of megaparsec. |
| `parser-combinators` | `parser-combinators-1.3.1`      | megaparsec companion. |
| `string-conversions` | `string-conversions-0.4.0.1`   | `cs` converters between `String`/`Text`/`ByteString`/`Html`. |
| `blaze-html`       | `blaze-html-0.9.2.0`              | `Text.Blaze.Html5`, `Text.Blaze.Html5.Attributes`, `Text.Blaze.Html.Renderer.String`. |
| `blaze-markup`     | `blaze-markup-0.8.3.0`            | `Text.Blaze.Internal` (the `MarkupM` GADT), `Text.Blaze`. |
| `blaze-builder`    | `blaze-builder-0.4.4.1`           | Builder primitives under blaze. |

**All deps are already in `~/.cache/ihc/sources/`** (verified 2026-04-25,
96 packages cached). Transitive deps of deps (`hashable`, `primitive`,
`dlist`, `pretty`, `ghc-prim`, …) likewise present. Unit 1's
cache-priming script is the belt-and-braces path for cold environments
(fresh CI, new developer).

---

## 4. Lexer / parser status

Lexer already tokenises `[hsx|…|]` correctly.
`src/IHC/Lexer.hs:600-644` handles the `[<lowercase-ident>|` prefix: TH
short-forms `[e|` / `[d|` / `[t|` / `[p|` / `[e||` emit `TkOQuote*`;
anything else (`[hsx|`, `[i|`, `[sql|`, …) emits `TkQQOpen <name>`. The
body is not scanned — left as opaque bytes up to `|]`.

Parser emits a real `EQuasiQuote` for `TkQQOpen`
(`src/IHC/Parser.hs`, `TkQQOpen` branch):

```haskell
-- [name| ... |] — QuasiQuoter. Capture body bytes, emit EQuasiQuote.
TkQQOpen qqName -> do
    curEnd <- skipQQBody ctx cur1
    let body = sliceBytes (ctxSrc ctx) (cPos cur1, cPos curEnd - 2)
    pure (EQuasiQuote qqName body, curEnd)
```

**Unit 3 done.** Body bytes are delivered to the evaluator, which
projects `quoteExp` and expands the quote at run time.

---

## 5. TH / QQ runtime path

End-to-end for one `[hsx|<h1>Hello world</h1>|]`:

1. **Lex** — `src/IHC/Lexer.hs:606-637` emits `TkQQOpen "hsx"`, then
   raw body bytes up to `|]`.
2. **Parse** — produces `EQuasiQuote name body` (`name = "hsx"`,
   `body = "<h1>Hello world</h1>"`).
3. **Resolve QQ** — evaluator looks up `IHP.HSX.QQ.hsx :: QuasiQuoter`.
   That's a record with `quoteExp, quotePat, quoteDec, quoteType`;
   expression position picks `quoteExp` via `$fldProj$quoteExp` and
   applies it to the body `String`.
4. **Run `quoteExp`** — `IHP.HSX.QQ.quoteHsxExpression` runs in `Q`
   (represented as `VIO`): calls `IHP.HSX.Parser.parseHsx` (megaparsec)
   on the body, walks the HSX AST, emits a TH `Exp` that constructs a
   `Text.Blaze.Internal.MarkupM`.
5. **TH Exp → IHC Expr** — `IHC.TH.thExpToExpr` (hook installed every
   `loadProgramFromSource` run) converts `VCon "LitE" …`, `VCon
   "AppE" …`, etc. back into `IHC.AST.Expr`.
6. **Evaluate** — normal `eval` / `force` loop; QQ dispatch is the
   `EQuasiQuote` case in `src/IHC/Eval.hs`.
7. **Render** — interpreted `Text.Blaze.Html.Renderer.String.renderHtml`
   walks the `MarkupM` tree to a `String`.
8. **Print** — `putStrLn`.

Key files:

| File | Responsibility |
|---|---|
| `src/IHC/Lexer.hs:600-644` | `TkQQOpen` tokenisation. |
| `src/IHC/Parser.hs:2867-2880` | Placeholder today, `EQuasiQuote` after Unit 3. |
| `src/IHC/AST.hs` | New `EQuasiQuote` constructor lands with Unit 3. |
| `src/IHC/Eval.hs:286-295` | Splice / quote expansion bridge. |
| `src/IHC/TH.hs` | TH Exp encoding / decoding (`thExpToExpr`, `expandSplicesInExpr`). |
| `~/.cache/ihc/sources/ihp-hsx-1.5.0/blaze/IHP/HSX/QQ.hs` | Interpreted `hsx`. |
| `~/.cache/ihc/sources/blaze-html-0.9.2.0/src/Text/Blaze/Html/Renderer/String.hs` | Interpreted `renderHtml`. |

---

## 6. Known blockers

### Landed (Unit 3 / QQ foundation)

- **Parser `EQuasiQuote` node.** Done — `TkQQOpen` → `EQuasiQuote name bodyBytes`.
- **Eval QQ dispatch.** Done — look up quoter, `$fldProj$quoteExp`, apply
  body `String`, `runIOVal`, `thExpToExpr`, re-eval. Fixture:
  `test/Fixtures/Coverage/qq_toy_string.hs`.
- **`thExpToExpr` hook on run-file path.** Done — installed in
  `loadProgramFromSource` (was only wired for the REPL via `buildBaseEnv`).
- **`QuasiQuoter` from source.** Done for template-haskell **2.22.x**
  (`Language.Haskell.TH.Quote` self-contained `data QuasiQuoter`).
  Cache via `scripts/cache-hsx-deps.sh` (now lists `template-haskell`).
  Prefer 2.22 over 2.24+: 2.24 re-exports from `GHC.Boot.TH.Quote`,
  which is not in the IHC source cache.

### Still open for full `[hsx|…]`

- **TH surface used by HSX `quoteHsxExpression`.** `location` /
  `extsEnabled` stubs exist; nested brackets + `$(pure expr)` antiquotes,
  `Lift Text`, and full megaparsec HSX parse under the interpreter remain
  the main risk surface. See `docs/HSX-TH-NEEDS.md`.
- **TH `reify`.** Not needed for hello-world; flagged for larger
  templates that inspect types via splices.
- **Class dispatch: `ToHtml` / `ToMarkup` / `ToValue`.** HSX attribute
  conversion goes through these. `ClassRegistry` needs entries on
  `String`, `Text`, `Int`, `Html`. Unit 6 territory.
- **Renderer primops.** `blaze-html`'s renderer is pure Haskell — no
  new primop *should* be needed. Unverified; a tight inner loop might
  hit a `ByteArray#`/`MutableByteArray#` primop we haven't implemented.
  If so: implement the primop, don't shim.
- **megaparsec under the interpreter.** `IHP.HSX.Parser` sits on
  megaparsec. Mostly pure Haskell; the concern is
  `unsafeDupablePerformIO` internals. Partial progress (Stream dispatch);
  `examples/hsx_hello` still expected-fail / nonterminates.
- **Data.Text builtins.** Text is cached and partially exercised. Add
  missing UTF-8 primops rather than shim.
- **`PackageImports`.** `IHP.HSX.QQ` imports `"template-haskell"
  Language.Haskell.TH`. Needs to round-trip through the loader.
- **GHC API in `IHP.HSX.HaskellParser`.** Antiquotations that parse
  embedded Haskell via the real GHC parser cannot run under IHC — see
  gap 29 in `docs/HSX-TH-NEEDS.md`. Hello-world with no `{…}` embeds
  may avoid this.
- **IHP parser gaps.** `ihp-parser-gaps.md` lists open parser issues.
  Fresh probe of `ihp-hsx` needed once its source hits the loader.

---

## 7. Milestone checklist

Ordered, one-PR-per-step:

1. **(Unit 1, done)** Cache-priming script `scripts/cache-hsx-deps.sh`
   for `ihp-hsx` + blaze + megaparsec + string-conversions +
   `template-haskell` (2.22 preferred).
2. **(Unit 2, done — expected-fail)** Smoke fixtures:
   `examples/hsx_hello/`, blaze hello baseline.
3. **(Unit 3, done)** Parser emits `EQuasiQuote name body`; AST /
   Pretty / free-var / rewrite paths carry the node.
4. **(done)** `EQuasiQuote` dispatch in `IHC.Eval` + `thExpToExpr` hook
   on the run-file path. Proven by `test/Fixtures/Coverage/qq_toy_string.hs`.
5. **(done for toy)** `QuasiQuoter` record construction/destruction via
   source-loaded `Language.Haskell.TH.Quote` (2.22).
6. Enough of `Q` for full HSX `quoteHsxExpression` (megaparsec parse,
   nested brackets, antiquotes) — still open.
7. `ToHtml` / `ToMarkup` / `ToValue` registry entries for `String`,
   `Text`, `Int`, `Html`.
8. Interpret `Text.Blaze.Html.Renderer.String.renderHtml` end-to-end;
   fix any missing primop / class dispatch encountered.
9. Green the Unit 2 / `hsx_hello` fixture (flip the expected-failure marker).
10. Document surprises under §6 and open follow-ups for splices with
    interpolation, runtime-`Text` attribute conversion, etc.
