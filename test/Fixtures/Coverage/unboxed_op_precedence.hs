-- Regression: GHC.Prim unboxed operators (@+#@, @-#@, @*#@, @==#@, …) have
-- no .hs source, so their fixities are never scanned from a module — they
-- must be seeded in the parser's defaultFixityTable.  Without the seed both
-- @==#@ and @-#@ default to (AssocL, 9), so a mixed expression like
-- @i# ==# n# -# 1#@ mis-parses as @(i# ==# n#) -# 1#@.
--
-- That silently broke GHC.Arr.listArray's fill loop
--   go y r = \i# s# -> case writeArray# marr# i# y s# of
--              s' -> if isTrue# (i# ==# n# -# 1#) then s' else r (i# +# 1#) s'
-- — the mis-parse made the "last index?" test fire immediately, so the loop
-- stopped after the first element and every multi-element array dropped its
-- last entry (`(Array.!): undefined array element`).  Fixities now match
-- GHC's primops.txt.pp (@-#@/@+#@ infixl 6, @*#@ infixl 7, comparisons infix 4).
{-# LANGUAGE MagicHash #-}
import GHC.Exts

main :: IO ()
main = do
    print (isTrue# (0# ==# 2# -# 1#))   -- 0 == (2-1) = 0 == 1  -> False
    print (isTrue# (1# ==# 2# -# 1#))   -- 1 == 1               -> True
    print (isTrue# (3# <# 2# +# 2#))    -- 3 < (2+2) = 3 < 4    -> True
    print (I# (2# *# 3# +# 1#))         -- (2*3)+1 = 7  (*# binds tighter)
