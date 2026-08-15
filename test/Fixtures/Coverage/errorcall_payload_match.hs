-- raise# of `error` / `errorWithoutStackTrace` leaves a string-shaped payload (or
-- ErrorCallWithLocation).  A constructor-pattern catch handler must
-- match ErrorCall against that payload and print the message, not
-- leftover PatternMatchFail / re-raised IhcException.
import Control.Exception

onError :: ErrorCall -> IO ()
onError (ErrorCall s) = putStrLn s

main :: IO ()
main = do
    catch (evaluate (error "boom" :: Int) >> putStrLn "unreachable") onError
    catch (evaluate (errorWithoutStackTrace "nostack" :: Int) >> putStrLn "unreachable") onError
