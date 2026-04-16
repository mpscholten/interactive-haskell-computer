# ast_eval

A tiny expression-language evaluator. Defines an `Expr` ADT with constructors
`Lit`, `Add`, `Mul`, `Neg`, `IfZ` and evaluates it via mutual recursion.

Demonstrates: user-defined ADTs, pattern matching on multiple constructors,
recursion, string concatenation, `show` on integers.

## Run

```
nix develop -c cabal run exe:ihc -- run examples/ast_eval/Main.hs
```

## Verify

```bash
nix develop -c cabal run exe:ihc -- run examples/ast_eval/Main.hs | diff - examples/ast_eval/ast_eval.out
```
