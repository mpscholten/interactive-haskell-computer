# Cold-start latency benchmark

Measures the wall-clock time from "process starts" to "program output appears"
for three runners on three representative Haskell programs.

## How to run

```sh
# From the project root:
./bench/cold-start.sh

# Or with specific programs:
./bench/cold-start.sh bench/programs/tiny.hs bench/programs/fib.hs
```

Results are printed as a Markdown table and also written to
`bench/results-YYYY-MM-DD.md`.

## Prerequisites

- macOS (uses `/usr/bin/time -p` BSD timing).
- `nix` with flakes enabled — the script calls `nix develop -c <runner>` for
  ghci and runghc, so the nix binary is the only hard dependency.
- An `ihc` binary at `dist-newstyle/.../ihc`.  If not present the script runs
  `nix develop -c cabal build ihc` automatically, but **this build itself takes
  several minutes and inflates the first ihc measurement** — build manually
  first if you want clean numbers:

  ```sh
  nix develop -c cabal build ihc
  ./bench/cold-start.sh
  ```

## Columns

| column   | meaning |
|----------|---------|
| runner   | `ihc` = our interpreter; `ghci` = GHC interactive (cold start, `-e main`); `runghc` = `ghc --make` + run (no separate compile step, project-free) |
| real (s) | wall-clock elapsed seconds (average of runs 2–5; run 1 is discarded as warm-up) |
| user (s) | user-space CPU seconds |
| sys (s)  | kernel CPU seconds |
| note     | number of runs averaged |

Run 1 is discarded to let the nix store and file-system caches warm up.
Runs 2–5 are averaged.  Five total runs per (program, runner) combination.

## Benchmark programs

| file | intent |
|------|--------|
| `programs/tiny.hs` | Baseline: `main = putStrLn "hi"`.  Measures pure runtime/loader overhead. |
| `programs/fib.hs`  | Compute-bound but tiny: naive `fib 25`.  Tests that startup cost dominates. |
| `programs/many_bindings.hs` | 100 unused top-level definitions + `main = print 42`.  GHC parses and type-checks all 100; ihc only touches `main`. |

## Known limitations

- macOS `/usr/bin/time -p` resolution is ~10 ms.  Programs that finish in under
  50 ms will have high relative noise (±20 %).  Average over more runs if needed.
- `nix develop -c ghci` pays the Nix shell start-up cost on every invocation
  (we intentionally do not keep a resident ghci process — that would measure
  something different).  This is the measurement: the cost a developer or CI
  system actually pays for each cold start.
- `runghc` on macOS does a full `ghc --make` compile into a temp dir each time.
  The object files are discarded; this is **not** incremental compilation.
- Results vary ±5–15 % between machines and under load.  Run on an idle machine
  with the display off for the most stable numbers.

## Multi-module / IHP-scale benchmarks

Once Phase 2.7 Cabal-aware loading is exercised against a real IHP project, add
an `ihp-hello` program here that imports several IHP modules.  The expectation is
that `ihc` loads only the modules reachable from `main`, while GHC eagerly
compiles everything.  At that scale the cold-start difference should be minutes
vs seconds.
