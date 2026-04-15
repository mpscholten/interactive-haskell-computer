-- A do-block with explicit braces sequences three IO actions.
main = do { putStrLn "first" ; putStrLn "second" ; print (3 * 4) }
-- expects:
--   first
--   second
--   12
