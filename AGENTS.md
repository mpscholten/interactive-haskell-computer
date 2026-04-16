# Interactive Haskell Computer

We are building a very efficient super fast haskell interpreter.

It should be able to run any haskell application on hackage. Specifically IHP and warp-based server apps.

It should effienctly use all cores given to the intrepter.
In interpreter mode it should delay type checking as far as possible.
It should be optimistic and should rely on the property that most haskell code is well typed most of the time.


One problem of GHC is that it does to much work all the time.

E.g. lets say we have a program:
```haskell
main = putStrLn "hello"

a = ...
b = ...
c = ..
```

Our intrepter should only parse, rename, and run `main` bindings, but ignore a, b, c (maybe type check im the background once main is runnoing)


Our compiler target is macOS only. Our interpreter loop should be a highly efficient aarch64 asm code (maybe written as direct assembly instead of haskell).

Ideally we run the haskell without much intermrediate layers. We should be inspiured of the old very fast pascal compilers.

## Builtin modules: minimum surface only

Keep the set of host-backed / "builtin" modules to an absolute minimum. The only modules that may be host-backed are those with **no Haskell source at all** — the compiler-built modules like `GHC.Prim`, `GHC.Types`, `GHC.Magic`. Anything with real `.hs` source in `base` (or in any Hackage package) must be interpreted from that source, not short-circuited via a builtin shim.

### Rules for adding a new builtin or builtin-backed module

Before adding anything to `isBuiltinBackedModule` or the primop catalog in `IHC.Builtins`, ALL of the following must hold:

1. **No source.** The module/symbol must have no `.hs` source in the relevant package (check `~/.cache/ihc/sources/<pkg>-<version>/`). If source exists, interpret it.
2. **Compiler-intrinsic OR RTS-exclusive.** The symbol is either a GHC primop (`GHC.Prim`), a compiler-built type (`GHC.Types`), or something that inherently can only live in the host RTS (e.g. a `ForeignPtr` allocation — no userland Haskell code could implement the underlying allocator).
3. **Documented justification.** Every whitelist entry in `isBuiltinBackedModule` must carry an inline comment explaining *why* it can't be source-loaded. "It's more convenient" is not a valid reason.
4. **No shims for ordinary Hackage libraries.** Do NOT host-shim `tasty`, `optparse-applicative`, `containers`, `Data.Text`, `aeson`, etc. — even if it's faster to ship. Those are ordinary Haskell; we interpret them. (See the Phase 2.10b abandonment and the tasty/optparse shim removal for precedent.)

If interpreting a module from source reveals a missing language extension, primop, or class-dispatch case, the correct response is to **implement the missing feature**, not to add another shim. This keeps the interpreter honest and exercises the full parser/evaluator path.