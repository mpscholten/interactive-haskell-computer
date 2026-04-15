# Interactive Haskell Computer

We are building a very efficient super fast haskell interpreter.

It should be able to run any haskell application on hackage. Specifically IHP and warp-based server apps.

It should effienctly use all cores given to the intrepter.
In interpreter mode it should delay type checking as far as possible.
It should be optimistic and should rely on the property that most haskell code is well typed most of the time.

