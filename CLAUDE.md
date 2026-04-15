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