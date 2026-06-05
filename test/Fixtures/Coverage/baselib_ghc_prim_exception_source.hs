-- Builtins-removal regression: GHC.Prim.Exception has source wrappers
-- around raise*# primops, so the module should source-load.
import GHC.Prim.Exception ()

main :: IO ()
main = putStrLn "ok"
