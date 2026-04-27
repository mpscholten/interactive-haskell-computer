#!/usr/bin/env bash
# Populate ~/.cache/ihc/sources/ with the HSX-rendering packages IHC needs in
# order to interpret `[hsx|…|]` quasi-quotes and render them as HTML.
#
# Per the CLAUDE.md "minimum surface" rule, `ihp-hsx`, the `blaze-*` family,
# and `megaparsec` must be interpreted from source, NOT host-shimmed.  The
# source tarballs live in the user-local cache at
# ~/.cache/ihc/sources/<pkg>-<ver>/ so the nix flake's ihcSourceRoot does not
# include them — each developer fetches them once with this script.
#
# Usage:
#   scripts/cache-hsx-deps.sh             # fetch the default set (HSX family)
#   scripts/cache-hsx-deps.sh <pkg> …     # also fetch extra packages by name
#
# Requires: cabal-install (network access to Hackage).
#
# Idempotent: skips packages whose cache directory already exists for any
# version.  To force a refresh, delete the existing directory first.
set -euo pipefail

SOURCES_DIR="${IHC_CACHE:-$HOME/.cache/ihc/sources}"
mkdir -p "$SOURCES_DIR"

# Default fetch list.  These are the HSX-rendering packages needed so that an
# interpreted program containing `[hsx|<h1>Hello</h1>|]` can parse the quote,
# build the resulting `Html` value, and render it to a `ByteString`.
#
#   ihp-hsx             — provides the `hsx` quasi-quoter itself
#   blaze-html          — the `Html` type and HTML combinators used by `hsx`
#   blaze-markup        — markup primitives that blaze-html builds on
#   blaze-builder       — ByteString builder backend for blaze rendering
#   megaparsec          — the parser `ihp-hsx` uses to lex the quote contents
#   string-conversions  — `cs` conversions used in HSX attribute handling
#   parser-combinators  — megaparsec's applicative-combinator companion
#   case-insensitive    — transitive dep; already cached, included defensively
DEFAULT_PKGS=(
    ihp-hsx
    blaze-html
    blaze-markup
    blaze-builder
    megaparsec
    string-conversions
    parser-combinators
    case-insensitive
)

PKGS=("${DEFAULT_PKGS[@]}" "$@")

fetched=0
skipped=0
failed=()

for pkg in "${PKGS[@]}"; do
    # Already present (any version)?  cabal get would create a new sibling
    # directory; we prefer to keep the existing one untouched.
    existing=$(ls -d "${SOURCES_DIR}/${pkg}-"[0-9]* 2>/dev/null | head -1 || true)
    if [ -n "$existing" ]; then
        echo "[cache-hsx-deps] skip ${pkg} — already present: ${existing}"
        skipped=$((skipped+1))
        continue
    fi

    echo "[cache-hsx-deps] fetching ${pkg}…"
    if (cd "$SOURCES_DIR" && cabal get "$pkg"); then
        fetched=$((fetched+1))
    else
        failed+=("$pkg")
    fi
done

echo ""
echo "[cache-hsx-deps] done: ${fetched} fetched, ${skipped} already present, ${#failed[@]} failed."
if [ ${#failed[@]} -gt 0 ]; then
    echo "[cache-hsx-deps] failed: ${failed[*]}" >&2
    exit 1
fi
