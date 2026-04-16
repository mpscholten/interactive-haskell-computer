# counter_ioref

Interactive counter that reads commands from stdin and maintains state in an `IORef`.
Commands: `inc` (increment), `dec` (decrement), `get` (print current value), `quit` (exit).

Demonstrates: `IORef`, `getLine`, string equality dispatch, mutable state, IO loops.

Note: string literal pattern matching in `case` is not yet supported; this example
uses `if/else` chains with `==` for command dispatch instead.

## Run

```
nix develop -c cabal run exe:ihc -- run examples/counter_ioref/Main.hs
```

Interactive example — type commands one per line. Or pipe input:

```bash
echo -e "inc\ninc\ndec\nget\nquit" | nix develop -c cabal run exe:ihc -- run examples/counter_ioref/Main.hs
```

## Verify

```bash
echo -e "inc\ninc\ndec\nget\nquit" | nix develop -c cabal run exe:ihc -- run examples/counter_ioref/Main.hs | diff - examples/counter_ioref/counter_ioref.out
```
