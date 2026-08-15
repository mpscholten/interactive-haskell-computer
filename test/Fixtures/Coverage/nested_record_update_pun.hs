-- Nested record update with NamedFieldPuns, matching
--   state { statePosState = (statePosState state) { pstateSourcePos } }
-- Custom ADT: no setPosition / State / PosState / megaparsec names.
data Inner = Inner { field :: Int, extra :: Int }
data Outer = Outer { inner :: Inner, tag :: Int }

setField field = \state -> state {
        inner = (inner state) { field }
    }

main :: IO ()
main = do
    let i = Inner { field = 1, extra = 2 }
        s = Outer { inner = i, tag = 3 }
        s' = setField 9 s
    print (field (inner s'))
    print (extra (inner s'))
    print (tag s')
