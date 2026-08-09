#!/usr/bin/env bash
# Benchmark owner-scoped expected-type metadata lookup.

set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
PROJECT_ROOT=$(cd "$SCRIPT_DIR/.." && pwd)
BUILD_DIR=${IHC_BUILDDIR:-dist-newstyle}
IHC_BIN=${IHC_BIN:-}
THRESHOLD=${IHC_THRESHOLD_SECONDS:-23}
SAMPLES=5

usage() {
    cat <<'EOF'
Usage: bench/expected-type-metadata.sh [OPTIONS]

Options:
  --binary PATH       Use an existing ihc executable instead of building.
  --builddir PATH     Cabal build directory (default: dist-newstyle).
  --threshold SEC     Fail if the median exceeds SEC (default: 23).
  -h, --help          Show this help.

The IHC_BIN, IHC_BUILDDIR, and IHC_THRESHOLD_SECONDS environment variables
provide the same configuration. Command-line options take precedence.
EOF
}

while (($#)); do
    case "$1" in
        --binary)
            [[ $# -ge 2 ]] || { echo "--binary requires a path" >&2; exit 2; }
            IHC_BIN=$2
            shift 2
            ;;
        --builddir)
            [[ $# -ge 2 ]] || { echo "--builddir requires a path" >&2; exit 2; }
            BUILD_DIR=$2
            shift 2
            ;;
        --threshold)
            [[ $# -ge 2 ]] || { echo "--threshold requires seconds" >&2; exit 2; }
            THRESHOLD=$2
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "unknown option: $1" >&2
            usage >&2
            exit 2
            ;;
    esac
done

command -v perl >/dev/null || {
    echo "perl is required for portable high-resolution timing; run this in nix develop" >&2
    exit 2
}
perl -e '$x = shift; die "threshold must be a positive number\n"
    unless $x =~ /\A(?:\d+(?:\.\d*)?|\.\d+)\z/ && $x > 0' "$THRESHOLD"

if [[ -z "$IHC_BIN" ]]; then
    echo "Building ihc once (builddir: $BUILD_DIR)..." >&2
    (cd "$PROJECT_ROOT" && nix develop -c cabal build exe:ihc --builddir "$BUILD_DIR")
    IHC_BIN=$(cd "$PROJECT_ROOT" && nix develop -c cabal list-bin exe:ihc --builddir "$BUILD_DIR")
fi

if [[ ! -x "$IHC_BIN" ]]; then
    echo "ihc binary is not executable: $IHC_BIN" >&2
    exit 2
fi

CONVERSION_FIXTURE="$PROJECT_ROOT/test/Fixtures/Coverage/expected_arg_cs_text.hs"
MPTC_FIXTURE="$PROJECT_ROOT/test/Fixtures/Coverage/class_mptc_typeapps.hs"
CONVERSION_EXPECTED='hello'
MPTC_EXPECTED=$(printf 'name=42\nage=30\nname[tag]\nage[tag]')
TMP_DIR=$(mktemp -d "${TMPDIR:-/tmp}/ihc-expected-type-bench.XXXXXX")
cleanup() {
    rm -f "$TMP_DIR/mptc.out" "$TMP_DIR/warmup.out"
    for ((cleanup_sample = 1; cleanup_sample <= SAMPLES; cleanup_sample++)); do
        rm -f "$TMP_DIR/sample-$cleanup_sample.out"
    done
    rmdir "$TMP_DIR"
}
trap cleanup EXIT

run_and_validate() {
    local fixture=$1 expected=$2 label=$3 output_file=$4
    if ! "$IHC_BIN" run "$fixture" >"$output_file"; then
        echo "$label failed" >&2
        return 1
    fi
    local actual
    actual=$(cat "$output_file")
    if [[ "$actual" != "$expected" ]]; then
        echo "$label produced unexpected output" >&2
        printf 'expected:\n%s\nactual:\n%s\n' "$expected" "$actual" >&2
        return 1
    fi
}

echo "Checking neutral multi-parameter type-class behavior..." >&2
run_and_validate "$MPTC_FIXTURE" "$MPTC_EXPECTED" "neutral MPTC fixture" "$TMP_DIR/mptc.out"

echo "Warming metadata and filesystem caches..." >&2
run_and_validate "$CONVERSION_FIXTURE" "$CONVERSION_EXPECTED" "conversion warm-up" "$TMP_DIR/warmup.out"

samples=()
for ((sample = 1; sample <= SAMPLES; sample++)); do
    start=$(perl -MTime::HiRes=time -e 'print time')
    run_and_validate "$CONVERSION_FIXTURE" "$CONVERSION_EXPECTED" \
        "conversion sample $sample" "$TMP_DIR/sample-$sample.out"
    end=$(perl -MTime::HiRes=time -e 'print time')
    elapsed=$(perl -e 'printf "%.3f", $ARGV[1] - $ARGV[0]' "$start" "$end")
    samples+=("$elapsed")
    printf 'sample %d/%d: %ss\n' "$sample" "$SAMPLES" "$elapsed"
done

median=$(printf '%s\n' "${samples[@]}" | sort -n | sed -n '3p')
printf 'median: %ss (threshold: %ss)\n' "$median" "$THRESHOLD"

perl -e '($median, $threshold) = @ARGV;
    die sprintf("benchmark regression: median %.3fs exceeds %.3fs\n", $median, $threshold)
        if $median > $threshold' "$median" "$THRESHOLD"
