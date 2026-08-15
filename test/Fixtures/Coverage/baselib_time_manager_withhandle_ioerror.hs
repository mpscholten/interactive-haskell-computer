-- TimeManager.withHandle is `E.handle ignore` with
--   ignore TimeoutThread = return Nothing
-- A non-TimeoutThread IOError (the Warp socket leftover shape) must
-- propagate, not become PatternMatchFail on TimeoutThread.

import Control.Exception
import Data.List (isInfixOf)
import System.TimeManager

classify :: SomeException -> String
classify e
    | "Non-exhaustive patterns in function" `isInfixOf` show e = "pmf"
    | otherwise =
        case fromException e of
            Just (_ :: IOException) -> "ioe"
            Nothing                 -> "other"

main :: IO ()
main = do
    r <- try $ withHandle defaultManager (return ()) $ \_ ->
            throwIO (userError "inside-withHandle")
    case r of
        Left e        -> putStrLn ("withHandle: " ++ classify e)
        Right Nothing -> putStrLn "withHandle: timeout"
        Right (Just ()) -> putStrLn "withHandle: ok"
