#!/usr/bin/env bash
# Populate ~/.cache/ihc/sources/ with the test-framework packages IHC needs to
# interpret Hackage test suites (bytestring, aeson, attoparsec, …).
#
# Per the CLAUDE.md "minimum surface" rule, the tasty family must be
# interpreted from source, NOT host-shimmed.  The source tarballs live in the
# user-local cache at ~/.cache/ihc/sources/<pkg>-<ver>/ so the nix flake's
# ihcSourceRoot does not include them — each developer fetches them once with
# this script.
#
# Usage:
#   scripts/cache-test-deps.sh            # fetch the default set (tasty family)
#   scripts/cache-test-deps.sh <pkg> …    # also fetch extra packages by name
#
# Requires: cabal-install (network access to Hackage).
#
# Idempotent: skips packages whose cache directory already exists for any
# version.  To force a refresh, delete the existing directory first.
set -euo pipefail

SOURCES_DIR="${IHC_CACHE:-$HOME/.cache/ihc/sources}"
mkdir -p "$SOURCES_DIR"

# Default fetch list.  These are the test-framework packages that appear in
# the bytestring / aeson / attoparsec test suites but are NOT bundled in the
# nix flake's ihcHackageSourceCandidates and do NOT ship with GHC.
#
# Transitive deps that *do* ship with GHC (stm, unix, directory, filepath,
# containers, mtl, transformers, time, process) are provided by the flake's
# ghcBootSourceRoot.  Transitive deps that are Hackage packages but already in
# the nix bundle (tagged, optparse-applicative, random, QuickCheck, call-stack,
# HUnit) are picked up from IHC_NIX_SOURCE_DIR.
#
# Remaining transitive Hackage gaps (ansi-terminal, ansi-terminal-types,
# colour, unbounded-delays, generic-deriving, xml) are added here so that
# `cabal get` pulls the full closure needed to interpret
# `Test.Tasty.defaultMain`.
DEFAULT_PKGS=(
    tasty
    tasty-quickcheck
    tasty-hunit
    tasty-ant-xml
    ansi-terminal
    ansi-terminal-types
    colour
    unbounded-delays
    generic-deriving
    xml
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
        echo "[cache-test-deps] skip ${pkg} — already present: ${existing}"
        skipped=$((skipped+1))
        continue
    fi

    echo "[cache-test-deps] fetching ${pkg}…"
    if (cd "$SOURCES_DIR" && cabal get "$pkg"); then
        fetched=$((fetched+1))
    else
        failed+=("$pkg")
    fi
done

echo ""
echo "[cache-test-deps] done: ${fetched} fetched, ${skipped} already present, ${#failed[@]} failed."
if [ ${#failed[@]} -gt 0 ]; then
    echo "[cache-test-deps] failed: ${failed[*]}" >&2
    exit 1
fi
