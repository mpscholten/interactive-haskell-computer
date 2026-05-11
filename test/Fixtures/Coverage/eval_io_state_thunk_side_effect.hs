-- Regression: source-loaded IO actions that wire their side effect
-- into the *state* slot of the unboxed tuple result —
-- @(# setAddrRange# dest# size# byte# s, () #)@ in @fillBytes@,
-- @writeAddr# ... s@ in raw poke loops — used to silently no-op
-- because 'IHC.Eval.runIOVal' forced only the result thunk of the
-- 'VCon "(#,#)" [stT, resT]' shape, leaving 'stT' (which is what
-- actually evaluates the side-effecting primop chain) un-evaluated.
--
-- The most user-visible symptom was @Data.ByteString.Char8.replicate
-- 4 'a'@ producing @"\\NUL\\NUL\\NUL\\NUL"@ instead of @"aaaa"@: the
-- @mallocByteString 4@ buffer was zero-initialised, @fillBytes@'s
-- IO action returned 'VUnit' correctly, but its inner
-- @setAddrRange#@ memset never ran.  This fixture exercises that
-- exact path: 'BSC.replicate' goes through 'unsafeCreateFp' →
-- 'createFp' → user-supplied filler → 'fillBytes ptr c w' →
-- '@setAddrRange# dest# size# byte# s@'.  Pre-fix: zero bytes.
-- Post-fix: the requested byte.
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
    print (BSC.replicate 4 'a')      -- "aaaa"
    print (BSC.replicate 8 'Z')      -- "ZZZZZZZZ"
    print (BSC.replicate 0 'x')      -- ""  (boundary)
