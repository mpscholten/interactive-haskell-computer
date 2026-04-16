-- Deliberate parse error: `let x bad` — `bad` is where `=` is expected.
-- The parser should report line 2, col 9 (the `in` token).
main = let x bad
       in x
