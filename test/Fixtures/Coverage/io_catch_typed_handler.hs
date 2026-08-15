-- Typed catch handler: fromException @IOException of a user Boom
-- must return Nothing and re-raise as SomeException so outer try
-- wraps leftover IhcException instead of wrapping IhcException: Boom.
-- Source catch stamps the handler type; try must still catch the
-- re-raise. Custom ADT. No Exception-type name list.
import Control.Exception

data Boom = Boom String deriving Show
instance Exception Boom

onIoe :: IOException -> IO ()
onIoe _ = pure ()

main :: IO ()
main = do
    r1 <- try $ catch (throwIO (Boom "not-ioe")) onIoe
    case r1 of
        Left (_ :: SomeException) -> putStrLn "catch-ioe-boom: not-ioe"
        Right _ -> putStrLn "catch-ioe-boom: swallowed"
    r3 <- try $ catch (throwIO (userError "real-ioe")) onIoe
    case r3 of
        Left (_ :: SomeException) -> putStrLn "catch-ioe-real: escaped"
        Right _ -> putStrLn "catch-ioe-real: caught"
