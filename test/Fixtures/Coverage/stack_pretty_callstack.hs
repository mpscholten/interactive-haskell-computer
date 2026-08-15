-- Coverage for HasCallStack / prettyCallStack callStack as implicit
-- params. HasCallStack is the type synonym
--
--   type HasCallStack = (?callStack :: CallStack)
--
-- and callStack is the source binding `case ?callStack of ...`.
-- Must not die with `implicit parameter ?callStack is not in scope`.
-- IHC does not synthesise call-site frames, so prettyCallStack of the
-- empty-discharged ?callStack is "".  Wrapped as STACK[...] so the
-- golden is not a blank line (`print` would hit Show [Char] vs []).
import GHC.Stack (HasCallStack, callStack, prettyCallStack)

f :: HasCallStack => String
f = prettyCallStack callStack

main :: IO ()
main = putStrLn ("STACK[" ++ f ++ "]")
