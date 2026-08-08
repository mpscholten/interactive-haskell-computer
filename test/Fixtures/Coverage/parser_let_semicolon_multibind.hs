-- Semicolon multi-bind in one-line layout `let` (no braces).
-- Seen in: lens-5.3.6 Control/Lens/Traversal.hs (ipartsOf / iunsafePartsOf):
--   let b = inline l sell s; (is, as) = unzip (pins b) in outs b <$> indexed f …
-- Pre-fix: expected `in` in let-binding; saw TkSemi
--
-- Must be expression-level `let` (not do-let) — that is the path that
-- hit TkSemi before `in`.

main = do
    print (let b = [1, 2, 3]; (xs, ys) = unzip [(10, 100), (20, 200), (30, 300)] in (b, xs, ys))
    print (let a = 1; c = 2; d = 3 in a + c + d)
