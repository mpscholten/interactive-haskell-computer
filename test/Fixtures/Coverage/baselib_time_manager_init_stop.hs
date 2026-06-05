-- Builtins-removal regression: initialize/stopManager have ordinary
-- time-manager source bodies and should not be host-backed shims.
import qualified System.TimeManager as T

main :: IO ()
main = do
    mgr <- T.initialize 10
    T.stopManager mgr
    putStrLn "ok"
