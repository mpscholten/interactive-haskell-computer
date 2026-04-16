# hello

The smallest program that works: a single `putStrLn` in `main`.

Demonstrates: basic IO, the `main` entry point.

## Run

```
nix develop -c cabal run exe:ihc -- run examples/hello/Main.hs
```

Expected output: `hello`

## Verify

```bash
nix develop -c cabal run exe:ihc -- run examples/hello/Main.hs | diff - examples/hello/hello.out
```
