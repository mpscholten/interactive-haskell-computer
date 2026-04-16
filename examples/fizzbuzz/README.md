# fizzbuzz

Classic FizzBuzz 1..100. Uses `mod`, guards, `show`, and a tail-recursive loop.

Demonstrates: multi-clause guards, `mod`, string `show` of `Int`, explicit recursion.

## Run

```
nix develop -c cabal run exe:ihc -- run examples/fizzbuzz/Main.hs
```

## Verify

```bash
nix develop -c cabal run exe:ihc -- run examples/fizzbuzz/Main.hs | diff - examples/fizzbuzz/fizzbuzz.out
```
