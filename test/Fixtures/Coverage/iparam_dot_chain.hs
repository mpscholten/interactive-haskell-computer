-- Parser fix: ?ctx.field postfix dot-chain now parses correctly.
-- The dot-chain is desugared to field ?ctx.
data Ctx = Ctx { ctxName :: String }
greet _unused = ?ctx.ctxName
main = let ?ctx = Ctx { ctxName = "Alice" } in putStrLn (greet True)
