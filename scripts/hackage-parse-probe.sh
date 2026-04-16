#!/usr/bin/env bash
# Run the IHC parse probe over 6 Hackage packages.
# Packages: hasql, servant, lens, conduit, hasql-pool, servant-server
#
# Usage:
#   scripts/hackage-parse-probe.sh [--fetch]
#
#   --fetch   run `cabal get` first to download missing packages
#
# Requires: cabal build exe:ihc-parse-probe (already in ihc.cabal)
set -euo pipefail

SOURCES_DIR="${HOME}/.cache/ihc/sources"
PROBE="$(cabal list-bin exe:ihc-parse-probe 2>/dev/null || \
          find dist-newstyle -name ihc-parse-probe -type f | head -1)"

if [[ -z "$PROBE" ]]; then
    echo "ihc-parse-probe binary not found. Run: cabal build exe:ihc-parse-probe"
    exit 1
fi

if [[ "${1:-}" == "--fetch" ]]; then
    echo "Fetching packages..."
    cd "$SOURCES_DIR"
    cabal get hasql hasql-pool servant servant-server lens conduit
    cd -
fi

run_probe() {
    local pkg="$1"
    local dir="$2"
    local label="$3"
    echo ""
    echo "==================================================================="
    echo "=== $label ($pkg)"
    echo "==================================================================="
    if [[ ! -d "$dir" ]]; then
        echo "ERROR: directory not found: $dir"
        echo "Run with --fetch to download packages."
        return 1
    fi
    find "$dir" -name "*.hs" | "$PROBE"
}

# hasql: library source only (not tests)
run_probe "hasql-1.10.3" \
    "$SOURCES_DIR/hasql-1.10.3/src/library" \
    "hasql"

# servant: src/
run_probe "servant-0.20.3.0" \
    "$SOURCES_DIR/servant-0.20.3.0/src" \
    "servant"

# lens: src/Control/Lens (core modules, 35 files)
echo ""
echo "==================================================================="
echo "=== lens-5.3.6 (Control.Lens core, 35 files)"
echo "==================================================================="
find "$SOURCES_DIR/lens-5.3.6/src/Control/Lens" -name "*.hs" | head -35 | "$PROBE"

# conduit: src/
run_probe "conduit-1.3.6.1" \
    "$SOURCES_DIR/conduit-1.3.6.1/src" \
    "conduit"

# hasql-pool: library source only
run_probe "hasql-pool-1.4.2" \
    "$SOURCES_DIR/hasql-pool-1.4.2/src/library" \
    "hasql-pool"

# servant-server: src/
run_probe "servant-server-0.20.3.0" \
    "$SOURCES_DIR/servant-server-0.20.3.0/src" \
    "servant-server"
