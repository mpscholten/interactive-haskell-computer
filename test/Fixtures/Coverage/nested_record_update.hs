-- Nested record update: s { inner = i { field = x } }.
-- parseHsx setPosition is
--   state { statePosState = (statePosState state) { pstateSourcePos } }
-- and used to die as "record update: unknown constructor".
-- Custom ADT: no setPosition / State / PosState / megaparsec names.
data Inner = Inner { field :: Int, extra :: Int }
data Outer = Outer { inner :: Inner, tag :: Int }

main :: IO ()
main = do
    let i = Inner { field = 1, extra = 2 }
        s = Outer { inner = i, tag = 3 }
        s' = s { inner = i { field = 9 } }
    print (field (inner s'))
    print (extra (inner s'))
    print (tag s')
