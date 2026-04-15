#!/usr/bin/env bash
# bench/cold-start.sh — cold-start latency benchmark for ihc vs ghci vs runghc
#
# Usage:  ./bench/cold-start.sh [FILE.hs ...]
#   With no arguments, runs the three programs in bench/programs/.
#
# Requirements:
#   - macOS with /usr/bin/time -p  (BSD time)
#   - nix develop shell *not* required to be active; the script calls
#     `nix develop -c <cmd>` itself so nix is the only hard dep.
#   - cabal must have already produced dist-newstyle/.../ihc  OR the
#     script will attempt `nix develop -c cabal build ihc` (slow, skews
#     the first ihc measurement — see README).
#
# Output:  markdown table on stdout + bench/results-YYYY-MM-DD.md

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
RESULTS_DIR="$SCRIPT_DIR"
DATE=$(date +%Y-%m-%d)
RESULTS_FILE="$RESULTS_DIR/results-${DATE}.md"
ITERATIONS=5   # run count per (program, runner) pair; first is discarded

# ── locate ihc binary ──────────────────────────────────────────────────────────
find_ihc() {
    find "$PROJECT_ROOT/dist-newstyle" -name "ihc" -type f 2>/dev/null \
        | head -1
}

IHC_BIN=$(find_ihc)
if [[ -z "$IHC_BIN" ]]; then
    echo "# ihc binary not found — building now (this skews the first measurement)" >&2
    (cd "$PROJECT_ROOT" && nix develop -c cabal build ihc 2>&1)
    IHC_BIN=$(find_ihc)
fi
if [[ -z "$IHC_BIN" ]]; then
    echo "ERROR: could not locate ihc binary after build attempt" >&2
    exit 1
fi
echo "# Using ihc: $IHC_BIN" >&2

# ── benchmark programs ─────────────────────────────────────────────────────────
if [[ $# -gt 0 ]]; then
    PROGRAMS=("$@")
else
    PROGRAMS=(
        "$SCRIPT_DIR/programs/tiny.hs"
        "$SCRIPT_DIR/programs/fib.hs"
        "$SCRIPT_DIR/programs/many_bindings.hs"
    )
fi

# ── timing helper ──────────────────────────────────────────────────────────────
# run_timed REAL_VAR USER_VAR SYS_VAR CMD [ARGS...]
# Sets named vars to the measured seconds.
# stdout of CMD is suppressed; stderr of CMD is discarded.
run_timed() {
    local _var_real="$1"; shift
    local _var_user="$1"; shift
    local _var_sys="$1";  shift

    local _tmpfile
    _tmpfile=$(mktemp /tmp/ihc-bench-time.XXXXXX)

    # Suppress stdout+stderr of the measured command; capture /usr/bin/time output.
    # BSD time writes to stderr; redirect CMD stderr to /dev/null separately.
    /usr/bin/time -p "$@" >/dev/null 2>"$_tmpfile" || true

    local _real _user _sys
    _real=$(grep '^real' "$_tmpfile" | awk '{print $2}')
    _user=$(grep '^user' "$_tmpfile" | awk '{print $2}')
    _sys=$(grep  '^sys'  "$_tmpfile" | awk '{print $2}')
    rm -f "$_tmpfile"

    eval "${_var_real}=${_real:-0}"
    eval "${_var_user}=${_user:-0}"
    eval "${_var_sys}=${_sys:-0}"
}

# average of space-separated decimal numbers
avg() {
    python3 -c "
import sys
nums = [float(x) for x in sys.argv[1:] if x]
print(f'{sum(nums)/len(nums):.3f}' if nums else '0.000')
" "$@"
}

# ── runner commands ────────────────────────────────────────────────────────────
# runner_cmd RUNNER FILE  →  sets global CMD array
runner_cmd() {
    local _runner="$1"
    local _file="$2"
    case "$_runner" in
        ihc)
            CMD=("$IHC_BIN" run "$_file")
            ;;
        ghci)
            CMD=(nix develop "$PROJECT_ROOT" -c ghci -v0 "$_file" -e 'main')
            ;;
        runghc)
            CMD=(nix develop "$PROJECT_ROOT" -c runghc "$_file")
            ;;
        *)
            echo "Unknown runner: $_runner" >&2; exit 1 ;;
    esac
}

RUNNERS=(ihc ghci runghc)

# ── output helpers ─────────────────────────────────────────────────────────────
tee_out() { echo "$*"; echo "$*" >> "$RESULTS_FILE"; }

# ── main benchmark loop ────────────────────────────────────────────────────────
{
    echo "# Cold-start latency benchmark"
    echo "# Generated: $(date)"
    echo "# Host: $(uname -m) $(uname -sr)"
    echo "# ihc: $IHC_BIN"
    echo ""
} > "$RESULTS_FILE"

echo "# Cold-start benchmark — $(date)" >&2
echo "# Programs: ${PROGRAMS[*]}" >&2

for PROG in "${PROGRAMS[@]}"; do
    PROG_NAME=$(basename "$PROG" .hs)
    echo "" >&2
    echo "## $PROG_NAME" >&2

    {
        echo "## $PROG_NAME"
        echo ""
        echo "| runner | real (s) | user (s) | sys (s) | note |"
        echo "|--------|----------|----------|---------|------|"
    } >> "$RESULTS_FILE"

    for RUNNER in "${RUNNERS[@]}"; do
        echo "   runner=$RUNNER  (${ITERATIONS} runs, dropping first)" >&2

        reals=()
        users=()
        syss=()
        status_ok=1

        for i in $(seq 1 "$ITERATIONS"); do
            CMD=()
            runner_cmd "$RUNNER" "$PROG"

            T_real=0; T_user=0; T_sys=0
            run_timed T_real T_user T_sys "${CMD[@]}" || {
                echo "      run $i: FAILED (${CMD[*]})" >&2
                status_ok=0
                break
            }

            echo "      run $i: real=${T_real}s user=${T_user}s sys=${T_sys}s" >&2

            # discard run 1 (warm-up)
            if [[ $i -gt 1 ]]; then
                reals+=("$T_real")
                users+=("$T_user")
                syss+=("$T_sys")
            fi
        done

        if [[ $status_ok -eq 0 ]]; then
            echo "| $RUNNER | FAILED | — | — | runner exited non-zero |" >> "$RESULTS_FILE"
        else
            AVG_REAL=$(avg "${reals[@]}")
            AVG_USER=$(avg "${users[@]}")
            AVG_SYS=$(avg  "${syss[@]}")
            echo "| $RUNNER | $AVG_REAL | $AVG_USER | $AVG_SYS | avg of runs 2-${ITERATIONS} |" >> "$RESULTS_FILE"
            echo "   -> avg real=${AVG_REAL}s" >&2
        fi
    done

    echo "" >> "$RESULTS_FILE"
done

echo "" >&2
echo "# Results written to $RESULTS_FILE" >&2
echo "" >&2

# ── print results to stdout ────────────────────────────────────────────────────
cat "$RESULTS_FILE"
