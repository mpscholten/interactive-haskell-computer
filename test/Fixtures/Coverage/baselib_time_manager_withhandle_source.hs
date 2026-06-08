-- Builtins-removal regression: withHandle/withHandleKillThread have
-- ordinary time-manager source bodies and should not be host-backed shims.
import qualified System.TimeManager as T

main :: IO ()
main = do
    T.withHandle T.defaultManager (putStrLn "timeout") $ \_ ->
        putStrLn "withHandle"
    mgr <- T.initialize 0
    T.withHandleKillThread mgr (putStrLn "timeout") $ \_ ->
        putStrLn "withHandleKillThread"
    putStrLn "ok"
