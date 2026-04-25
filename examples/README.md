# IHC Examples

Runnable programs that dogfood the interpreter today. Each runs end-to-end
under `ihc run`. Sorted from simplest to most advanced.

Usage for all examples:

```
nix develop -c cabal run exe:ihc -- run examples/<name>/Main.hs
```

| Example | Description |
|---------|-------------|
| [hello](hello/) | `main = putStrLn "hello"` — the smallest possible program |
| [fizzbuzz](fizzbuzz/) | Classic FizzBuzz 1..100 using `mod`, guards, `show` |
| [ast_eval](ast_eval/) | Tiny expression-language evaluator: user ADT + pattern matching + recursion |
| [counter_ioref](counter_ioref/) | Interactive `IORef` counter: `getLine` loop, mutable state, string dispatch |
| [fib_memo](fib_memo/) | Fibonacci with memoization via `IORef [(Int,Int)]` association list |
| [record_dot](record_dot/) | `OverloadedRecordDot`: `p.pName`, chained `p.pAddr.addrCity` access |
| [type_classes](type_classes/) | Custom `instance Show` and `instance Eq` for a user-defined `Color` type |
| [async_ping](async_ping/) | `forkIO` + `MVar` ping-pong between two threads; demonstrates Phase 2.10a concurrency |
| [hsx_hello](hsx_hello/) | **Expected-fail target.** `[hsx|<h1>Hello world</h1>|]` rendering via `IHP.HSX.QQ` + `Text.Blaze.Html.Renderer.String`. Errors today — wired as expected-fail in `RunFile.hs` until HSX quasi-quoting lands. |
| [blaze_hello](blaze_hello/) | **Expected-fail target.** `Text.Blaze.Html5` hello-world without the HSX QuasiQuoter. Errors today on blaze-markup's record accessor — wired as expected-fail in `RunFile.hs`. |

## Known limitations (as of writing)

- **Float literals**: `print 3.14` fails (`Prelude.read: no parse` in the lexer).
  Use integer arithmetic where possible.
- **String literal `case`**: `case s of { "foo" -> ... }` does not match.
  Use `if s == "foo"` chains instead.
- **Multi-line record syntax**: fields declared on separate lines are not scanned.
  Declare all fields on one line: `data T = T { fieldA :: Int, fieldB :: String }`.
- **Custom class dispatch**: `class Foo a where bar :: a -> Int` is parsed but
  `bar` is not injected as a dispatch function. Use `instance Show`/`instance Eq`
  or implement dispatch manually.
- **`import Data.IORef`**: triggers a deep base source-load that hits a missing
  `MachDeps.h`. Omit the import — IORef builtins are always in scope.
