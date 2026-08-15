-- Expression-level `let f = e where b = e2 in body` on one line.
-- Pre-fix: findWhereBlockEndAt swallowed `in` (same line, higher col)
-- so finishLetItems saw TkEof.  GHC.Exception leftover shape
-- `where ?callStack = stk` uses the same scanner.
main = print (let f = x where x = 7 in f)
