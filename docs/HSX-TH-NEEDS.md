# HSX TH/QQ Language-Feature Inventory

Sources analysed: `ihp-hsx-1.5.0` (available both from the development cache
and the installed executable-relative Nix source bundle).

Runtime status (verified 2026-08-10 with the PR #249 implementation snapshot
`7880e00`, merged as `ac577a5`, plus probe fix `60f7878`, merged at
`1348727`): the clean-source
`examples/hsx_hello` sentinel reaches the source quoter, then fails with
`thExpToExpr: unsupported TH Exp constructor: ParsecT`. This inventory must
therefore distinguish implemented focused fixtures from the still-broken
nested carrier path.

The package is split into three sub-libraries:

- `parser/IHP/HSX/{Parser,HaskellParser,HsExpToTH}.hs` — HSX body parser + GHC-API bridge that converts parsed Haskell fragments to `TH.Exp`.
- `blaze/IHP/HSX/{QQ,ToHtml,Attribute,ConvertibleStrings}.hs` — the `QuasiQuoter` definition, the `ToHtml` class, the Blaze compile-target.
- `lucid2/IHP/HSX/Lucid2/*.hs` — a parallel Lucid2 backend (structurally identical to the Blaze one; the user wants Blaze for the first milestone).

This doc is strictly a gap inventory. All references use absolute line numbers in the cached sources.

## Feature table

| # | Feature | Where used in `ihp-hsx` (file:line) | IHC status | IHC reference |
|---|---------|-------------------------------------|------------|----------------------------------|
| 1 | `QuasiQuoter` record construction with all four slots (`quoteExp`, `quotePat`, `quoteType`, `quoteDec`) | `blaze/IHP/HSX/QQ.hs:57-62`, re-exported as `hsx`/`uncheckedHsx`/`customHsx` | **done** | Parser emits `EQuasiQuote`; eval projects `$fldProj$quoteExp`. Fixture: `test/Fixtures/Coverage/qq_toy_string.hs`. |
| 2 | Body delivered to `quoteExp` as `String` | `blaze/IHP/HSX/QQ.hs:64` (`quoteHsxExpression :: HsxSettings -> String -> TH.ExpQ`) | **done** | `skipQQBody` + `sliceBytes` capture body; eval feeds `[Char]` via `stringLiteralToListVal`. |
| 3 | `Language.Haskell.TH.Quote.QuasiQuoter` constructor | `blaze/IHP/HSX/QQ.hs:20` (`import Language.Haskell.TH.Quote`) | **done** | `Language.Haskell.TH.Quote` and its constructor are source-loaded from the packaged Template Haskell sources; `thBuiltinPairs = []`. |
| 4 | `TH.ExpQ` / `Q` monad sequencing (`do`, `>>=`) inside quoter body | `blaze/IHP/HSX/QQ.hs:64-71`, `blaze/IHP/HSX/QQ.hs:244` | **partial, source-loaded** | Source `Q` actions now dispatch through source instances and are run at the TH-expression boundary; no ordinary-library `Q` shim remains. Focused source-Q and pure-splice fixtures are green. The real HSX run still loses the enclosing carrier and presents `ParsecT` to `thExpToExpr`. |
| 5 | `TH.location :: Q Loc` + `TH.loc_start`, `TH.loc_filename` field access | `blaze/IHP/HSX/QQ.hs:75-77` (`findHSXPosition`) | implemented for current source path | Source-backed runtime support is present and the real sentinel advances beyond this call. |
| 6 | `TH.extsEnabled :: Q [Extension]` | `blaze/IHP/HSX/QQ.hs:67` (`extensions <- TH.extsEnabled`) | implemented for current source path | The real sentinel advances beyond this call; the resulting extension list remains relevant to embedded-expression parsing. |
| 7 | Expression bracket `[\| … \|]` used extensively in the compile-to-Haskell step | `blaze/IHP/HSX/QQ.hs:80` (`Html5.docType`), `:91`, `:95`, `:99`, `:101-105`, `:114-206`, `:215-230`, `:244`, `:258`, `:264-265`, `:271`, `:275`, `:284-459`, `:466` | partial | Parser and evaluator support `EQuote`; `evalQuote` covers variables, literals, applications, negation, tuples, and nested `ESplice`. Other expression shapes must be added generically if the real HSX path reaches them. |
| 8 | Typed bracket `[|| … ||]` | not used in `ihp-hsx` | n/a | The parser currently emits an unsupported-quote placeholder. Not a blocker for HSX. |
| 9 | Declaration / type / pattern brackets `[d| |]`, `[t| |]`, `[p| |]` | not used in `ihp-hsx` | n/a | Same placeholder as gap 8. Not a blocker for HSX. |
| 10 | Untyped splice `$(…)` nested inside a bracket (antiquotation) | `blaze/IHP/HSX/QQ.hs:91`, `:95`, `:103`, `:244`, `:264-265`, `:275`, `:466` | **done in focused fixtures** | `ESplice` is parsed, expanded through `IHC.TH`, and explicitly evaluated inside `evalQuote`. Source-defined `$(pure expression)` is covered; full HSX remains gated by the carrier failure. |
| 11 | `pure :: Exp -> Q Exp` (applicative lift of a pre-built AST node into the quoter) | `blaze/IHP/HSX/QQ.hs:103` (`toHtml $(pure expression)`), `:264`, `:275` | **done in focused source-Q fixtures** | The annotated `Q` instance and source-defined pure quote-splice path are covered. The full HSX path is blocked earlier by loss of the nested signed carrier. |
| 12 | `Lift` instance for `Text` (the element/attribute tag names are `Text`) | `blaze/IHP/HSX/QQ.hs:244` (`$(TH.lift name)`), `:258`, `:466` | pending end-to-end verification | `Lift Text` must be selected from the real source-loaded `text` instance through ordinary class dispatch. The current sentinel fails before this can be classified as working or broken. |
| 13 | `TH.mkName` | not used directly in `ihp-hsx` source | n/a | Not a blocker for HSX. |
| 14 | `TH.reify` / `TH.lookupValueName` / `TH.lookupTypeName` | not used in `ihp-hsx` (HSX doesn't introspect) | n/a | Not a blocker for HSX hello-world. |
| 15 | `Haskell.Exp` (the `template-haskell` `Exp` ADT) as an intermediate value | `parser/IHP/HSX/Parser.hs:32-34` (import), `:44-55` (`AttributeValue`, `Node` store `Haskell.Exp`), `blaze/IHP/HSX/QQ.hs:103` passes it into `$(pure …)` | partial | The HSX parser builds ordinary source-loaded `Haskell.Exp` values and stores them inside `AttributeValue`/`SplicedNode`. `thBuiltinPairs = []`; Template Haskell constructors are not host builtin registrations. `thExpToExpr` must decode every source constructor shape that HSX can produce. A careful cross-check against `HsExpToTH.hs:38-262` remains after the carrier blocker — see gap 16. |
| 16 | `HsExpToTH.toExp` — runs inside the HSX parser, converts `HsExpr GhcPs` (parsed by the real GHC API) to `TH.Exp` | `parser/IHP/HSX/HsExpToTH.hs:92-252` | partial | The broader GHC API source-loading question remains. Its record-dot outputs are no longer missing: `GetFieldE` and chained `ProjectionE` decode generically into IHC expressions with focused fixtures. Other emitted constructors remain subject to end-to-end verification after the carrier fix. |
| 17 | `TH.Q` monad instance (`Monad`, `Applicative`, `Functor`, `MonadIO`, `Quote`) | implicit in `do` block inside `quoteHsxExpression` | partial | Instances are discovered and interpreted from Template Haskell source. `Applicative Q` and source `pure` are covered; full HSX currently demonstrates that carrier evidence is not retained through every nested/rank-polymorphic field. |
| 18 | `TH.Lift` class (dispatches on the type of the lifted value) | `blaze/IHP/HSX/QQ.hs:244`, `:258` (`TH.lift name` where `name :: Text`) | pending end-to-end verification | Template Haskell and `text` are source-loaded; `Lift` must follow their source class/instance path. See gap 12. |
| 19 | `(Text -> TH.ExpQ)` values stored in a `HashMap` (runtime dispatch on tag name) | `blaze/IHP/HSX/QQ.hs:111-207`, `:212-231`, `:281-459` | supported | Ordinary Haskell — requires `unordered-containers` / `hashable` to be source-interpreted. Not a TH concern. |
| 20 | `Data.String.Conversions.cs` (used throughout to coerce `Text`/`String`/`ByteString`) | `blaze/IHP/HSX/QQ.hs:25, 69, 239, 256`, many others | supported | Ordinary class dispatch. |
| 21 | `HsxSettings` with `ImplicitParams` (`?settings`, `?extensions`) inside the parser | `parser/IHP/HSX/Parser.hs:69-76`, `:222`, `:231`, `:306-307` | supported | `EImplicitRef` and `EImplicitLet` are implemented. |
| 22 | `Data.Set`, `Data.HashMap.Strict`, `Data.Containers.ListUtils.nubOrd` | throughout | supported | Ordinary Hackage code; source-interpreted. Not a TH concern. |
| 23 | `Text.Megaparsec` — HSX uses it at *runtime* inside `quoteHsxExpression` | `blaze/IHP/HSX/QQ.hs:27, 69, 77`, `parser/IHP/HSX/Parser.hs:24-25, 74` | partial | Megaparsec is bundled and source-interpreted. Multiple focused `Text` parser paths pass; the current real failure exposes a `ParsecT` value at the enclosing `Q Exp` boundary. |
| 24 | The HSX parser mentions `TH.Extension` in its embedded-expression signature | `parser/IHP/HSX/Parser.hs:33, 68`, `parser/IHP/HSX/HaskellParser.hs:13, 27` | source available; end-to-end pending | `TH.Extension` comes from packaged Template Haskell source. Its later use is not reached by the current hello sentinel. |
| 25 | Multiple records with `OverloadedStrings` for tag/attribute sets | `parser/IHP/HSX/Parser.hs:319-416, 418-592, 594-608` | supported | Ordinary Haskell. |
| 26 | `Lift` for `Text` used via `TH.lift name` in `nodeToBlazeElementGeneric` / `nodeToBlazeLeafGeneric` / `attributeFromNameGeneric` | `blaze/IHP/HSX/QQ.hs:244, 258, 466` | pending end-to-end verification | Same source-instance requirement as gaps 12 and 18; no host fallback is permitted. |
| 27 | `TH.ExpQ` alias (= `Q Exp`) pervasive in return types | `blaze/IHP/HSX/QQ.hs:64, 79, 107, 208, 232, 246, 260, 267, 277, 461` | partial, source-loaded | The alias and `Q` instances are interpreted from source. Focused `Q Exp` actions pass; nested carrier preservation in the full quoter remains open. |
| 28 | Record update inside a splice: `pos { sourceLine = mkPos line, sourceColumn = mkPos col }` (this is inside megaparsec, not TH, but flows through the HSX parser) | `parser/IHP/HSX/Parser.hs:188` | supported | `ERecordUpdate` is implemented. |
| 29 | GHC API parser (`GHC.Parser`, `GHC.Parser.Lexer`, `GHC.Data.StringBuffer`, `GHC.Types.SrcLoc`, `GHC.Hs.Expr`, etc.) | `parser/IHP/HSX/HaskellParser.hs:1-89`, `parser/IHP/HSX/HsExpToTH.hs:11-33` | not yet reached | These compiler-package modules require the same honest source/compiler-intrinsic treatment as any other dependency. The current no-antiquotation hello sentinel fails earlier, so this row records future surface rather than the active blocker. |
| 30 | Splicing a TH `Exp` returned by a `Q`-monad action (via `$()`) where the splice body is not a literal `[| |]` | `blaze/IHP/HSX/QQ.hs:103` (`$(pure expression)`), `:264`, `:275` — `expression :: Haskell.Exp` is a *value*, not a quotation | partial | Source-defined `$(pure expression)` works in focused fixtures. `GetFieldE` and `ProjectionE` are decoded. Full HSX remains blocked by the enclosing `Q`/`ParsecT` carrier mix-up before constructor completeness can be claimed. |

## Top 5 gaps (priority order)

1. **GHC API parser dependency** (gap 29) — **XL, not yet reached by the
   hello sentinel**. HSX's embedded-expression path imports the GHC parser
   package. The compliant route is to implement whatever compiler-package
   source/runtime support that path requires, or adapt IHC's general parser
   architecture without replacing an ordinary Hackage module. An HSX shim or
   install-time host preprocessing is prohibited.

2. **Lift instance for `Text`** (gaps 12, 18, 26) — **M, pending runtime
   verification**. Resolve `TH.lift name` through the real source-loaded
   `Lift Text` instance and ordinary class dispatch. Do not identify `Text` by
   runtime shape or add a host implementation.

3. **Preserve the signed nested carrier** (gaps 4, 17, 30) — **current observed blocker** — the sentinel reaches source quoter execution but `thExpToExpr` receives `ParsecT` instead of the enclosing `Q Exp` result. Fix carrier evidence through rank-polymorphic constructor fields/parser actions; do not special-case HSX or `ParsecT`.

4. **Wire user-defined `QuasiQuoter` records into the parser's `[name|…|]` routing** (gaps 1, 2, 3) — **DONE**. Parser emits `EQuasiQuote`; eval-time dispatch projects `quoteExp`. Proven by `qq_toy_string` Coverage fixture. Remaining work is HSX-specific (gaps above), not the QQ plumbing itself.

5. **Record-dot TH constructors: `ProjectionE`, `GetFieldE`** (gap 16,
   part of 10) — **DONE in focused fixtures**. Both decode generically, including
   chained projections. Reverify them through a real HSX antiquotation once
   the carrier blocker is removed.

## Notes on "implemented" vs "partial" vs "missing"

A feature is marked **implemented** only if the source-interpreted path in IHC covers the use ihp-hsx makes of it (not merely that a symbol exists).

A feature is marked **partial** if either (a) only a subset of constructors / cases is handled, or (b) the path works for simple inputs but misses HSX-relevant cases.

A feature is marked **missing** if there is no code path at all in `src/IHC/TH.hs`, `src/IHC/Eval.hs`, `src/IHC/AST.hs`, `src/IHC/Parser.hs`, or the class registry that would handle it.

The no-shims rule means `ihp-hsx`, Blaze, Megaparsec, `text`, and Template
Haskell declarations are interpreted from source. `thBuiltinPairs = []`;
`Exp`/`Pat`/`Lit` values arise from source constructors and `thExpToExpr`
decodes their runtime representation. The currently observed blocker is the
`Q`/`ParsecT` carrier boundary, before the embedded-expression GHC API path.
