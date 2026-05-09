-- Regression: a backticked MagicHash operator inside parens at body
-- position must NOT cause the binding scanner to lose the enclosing
-- top-level binding.
--
-- Reduction of the @ghc-prim-0.12.0/GHC/Classes.hs:301@ pattern that
-- was blocking @==@ removal:
--
--     (C# x) `eqChar` (C# y) = isTrue# (x `eqChar#` y)
--
-- Pre-fix, scanning that body broke discovery of @eqChar@ itself, so
-- @instance Eq Char where (==) = eqChar@ resolved to a method
-- placeholder and @'a' == 'b'@ couldn't dispatch through the source-
-- loaded @Eq Char@ instance.
--
-- The minimal trigger has nothing to do with class methods or
-- @eqChar#@ specifically — it's just @binding = (... `prim#` ...)@:
--
--     foo = (1 `xyz#` 2)
--     main = print foo
--
-- pre-fix output: @ihc: IHC.Eval: unbound variable 'foo'@.  Drop the
-- trailing @#@ on the backticked op and @foo@ binds; drop the
-- surrounding parens and @foo@ binds.  The trigger is specifically
-- @TkBacktick TkPrimId TkBacktick@ inside @(...)@ at body position.
--
-- This fixture pins the fix using a real working primop chain so the
-- output can be golden-checked rather than relying on the binding
-- silently being unreachable.

import GHC.Types  (Char(C#), isTrue#)
import GHC.Prim   (eqChar#)

myEqChar :: Char -> Char -> Bool
myEqChar (C# x) (C# y) = isTrue# (x `eqChar#` y)

main :: IO ()
main = do
    print (myEqChar 'a' 'a')
    print (myEqChar 'a' 'b')
