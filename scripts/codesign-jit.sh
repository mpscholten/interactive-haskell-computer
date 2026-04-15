#!/usr/bin/env bash
# Ad-hoc codesign a binary with the JIT entitlement.
#
# Usage: scripts/codesign-jit.sh PATH-TO-BINARY
#
# Must be re-run after every cabal build/test that produces a new executable,
# because MAP_JIT + hardened runtime refuses to toggle pages executable
# otherwise. We sign with identity "-" (ad-hoc) — fine for local dev; a real
# release would use a proper signing identity.
#
# cabal has no portable post-build hook, so the Makefile targets call this.

set -euo pipefail

if [ $# -ne 1 ]; then
    echo "usage: $0 PATH-TO-BINARY" >&2
    exit 2
fi

BIN="$1"
ENTITLEMENTS="$(cd "$(dirname "$0")/.." && pwd)/jit.entitlements"

if [ ! -f "$BIN" ]; then
    echo "codesign-jit: binary not found: $BIN" >&2
    exit 1
fi

if [ ! -f "$ENTITLEMENTS" ]; then
    echo "codesign-jit: entitlements file not found: $ENTITLEMENTS" >&2
    exit 1
fi

codesign --sign - \
         --force \
         --options runtime \
         --entitlements "$ENTITLEMENTS" \
         --timestamp=none \
         "$BIN"

echo "signed: $BIN"
