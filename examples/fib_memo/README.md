# fib_memo

Fibonacci 0..15 with memoization via an `IORef` holding an association list.
Avoids redundant recomputation by caching `(n, fib(n))` pairs.

Demonstrates: `IORef`, `modifyIORef'`, user-defined `lookup` on `[(Int, Int)]`,
`case` on `Maybe`, mutual recursion, `++` string building.

Note: `Data.IORef` is NOT imported — `newIORef`, `readIORef`, `modifyIORef'`
are provided as builtins. Importing `Data.IORef` would trigger a deep base
source-load that currently fails on `MachDeps.h`.

## Run

```
nix develop -c cabal run exe:ihc -- run examples/fib_memo/Main.hs
```

## Verify

```bash
nix develop -c cabal run exe:ihc -- run examples/fib_memo/Main.hs | diff - examples/fib_memo/fib_memo.out
```
