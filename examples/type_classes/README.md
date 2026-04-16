# type_classes

Custom `Show` and `Eq` instances for a user-defined `Color` type.
`show` dispatches through the class registry; `==` uses multi-clause
instance matching on nullary constructors.

Demonstrates: `instance Show`, `instance Eq`, class registry dispatch,
ADT with multiple constructors, polymorphic use of `show` and `==`.

Note: ihc supports `instance` declarations for the built-in `Eq` and `Show`
classes. Custom class declarations with custom methods (e.g. `class Shape a`)
are parsed but dispatch functions are not yet auto-generated — user-defined
class methods work only when implemented directly as top-level functions.

## Run

```
nix develop -c cabal run exe:ihc -- run examples/type_classes/Main.hs
```

## Verify

```bash
nix develop -c cabal run exe:ihc -- run examples/type_classes/Main.hs | diff - examples/type_classes/type_classes.out
```
