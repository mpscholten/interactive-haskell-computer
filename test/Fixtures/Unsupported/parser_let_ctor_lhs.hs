-- Gap: Constructor pattern on LHS of a `let` binding (`let Con f a = expr`). Seen in: lens-5.3.6/Control/Lens/Profunctor.hs:3:24, servant-server-0.20.3.0/Server/Internal/Context.hs:2:20. Ref: hackage-parser-gaps.md.
data Pair = Pair Int String

main = do
    let Pair n s = Pair 42 "hello"
    putStrLn s
    print n
