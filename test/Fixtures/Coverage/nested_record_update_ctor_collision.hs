-- Nested record update `s { inner = i { field = x } }` must keep the
-- local constructor's arity when another loaded module defines a
-- wider homonym of the same bare constructor name.
--
-- parseHsx setPosition is
--   state { statePosState = (statePosState state) { pstateSourcePos } }
-- and used to die as "record update: unknown constructor" once
-- unrelated `State` constructors from the loaded-module field-registry
-- union inflated the desugared pattern.  Custom ADT: no setPosition /
-- State / PosState / megaparsec names.
import Modules.NestedRecordUpdateCollision.Wide (wideName)

data Inner = Inner { field :: Int, extra :: Int }
data Outer = Outer { inner :: Inner, tag :: Int }

setField field = \state -> state {
        inner = (inner state) { field }
    }

main :: IO ()
main = do
    putStrLn wideName
    let i = Inner { field = 1, extra = 2 }
        s = Outer { inner = i, tag = 3 }
        s' = setField 9 s
    print (field (inner s'))
    print (extra (inner s'))
    print (tag s')
