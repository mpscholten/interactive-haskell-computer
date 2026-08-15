-- Polymorphic [] in a constructor field must be the nil constructor,
-- not a leftover class-method function (fromString / empty / mempty).
-- Parser [] and desugared "" share EVar "[]"; elaborating that at a
-- type-variable field type (Box a, ReaperSettings.reaperEmpty) left
-- <function>.  TimeManager.initialize / stopManager then mapM_'s the
-- leftover and hangs.  Custom ADT so we do not depend on Reaper.
data Box a = Box { boxVal :: a }

main :: IO ()
main = do
    case boxVal (Box { boxVal = [] }) of
        [] -> putStrLn "record-ok"
        _  -> putStrLn "record-fn"
    case (\(Box x) -> x) (Box []) of
        [] -> putStrLn "pos-ok"
        _  -> putStrLn "pos-fn"
