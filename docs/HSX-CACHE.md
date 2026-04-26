# HSX source cache

The IHC interpreter must be able to evaluate

```haskell
[hsx|<h1>Hello world</h1>|]
```

and render the resulting `Html` to a `ByteString`. That requires the HSX
quasi-quoter, the Blaze HTML machinery, and the parser stack used by
`ihp-hsx`. Because these packages are **ordinary Hackage libraries with real
`.hs` source**, the no-shim rule in `CLAUDE.md` (§ "Builtin modules: minimum
surface only") applies: we must interpret them from source — host-shimming
them is not an option.

This document records how that source is made available.

## Fetching the source

Run:

```
scripts/cache-hsx-deps.sh
```

It populates `~/.cache/ihc/sources/<pkg>-<ver>/` with `cabal get` for each
entry in its `DEFAULT_PKGS` list. The script is idempotent — re-running prints
`skip <pkg>` for everything that is already present and exits 0.

## Why each package is cached

| package              | role                                                           |
|----------------------|----------------------------------------------------------------|
| `ihp-hsx`            | provides the `[hsx|…|]` quasi-quoter itself                    |
| `blaze-html`         | the `Html` type and HTML combinators that `hsx` targets        |
| `blaze-markup`       | markup primitives that `blaze-html` is built on top of         |
| `blaze-builder`      | ByteString-builder backend used by Blaze's renderers           |
| `megaparsec`         | the parser `ihp-hsx` uses to lex the quote contents            |
| `string-conversions` | `cs` conversions used in HSX attribute handling                |
| `parser-combinators` | megaparsec's applicative-combinator companion (transitive dep) |
| `case-insensitive`   | transitive dep; already cached, included defensively           |

## Pre-existing cache state (before this change)

At the time this script was added, `~/.cache/ihc/sources/` already contained a
large number of packages from earlier slices — `aeson`, `attoparsec`,
`bytestring`, `text`, `vector`, `warp`, `wai`, `hspec-*`, `tasty-*`, the
`ghc-*` boot libs, and so on — but none of the HSX-rendering packages listed
above. `case-insensitive-1.2.1.0` was the only transitive dep in the list
that was already present; the other seven were fetched fresh.

## No shims — source only

Per `CLAUDE.md`:

> Do NOT host-shim `tasty`, `optparse-applicative`, `containers`, `Data.Text`,
> `aeson`, etc. — even if it's faster to ship. Those are ordinary Haskell; we
> interpret them.

The same applies to every package in this list. If interpreting any of them
exposes a missing language extension, primop, or class-dispatch case, the fix
is to implement the missing feature in the interpreter — **not** to add a
host shim for `ihp-hsx` or `blaze-html`. Caching source is the legitimate
path.
