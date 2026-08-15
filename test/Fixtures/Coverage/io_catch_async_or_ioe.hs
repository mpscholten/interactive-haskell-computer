-- Typed SomeAsyncException wrap after constructor-pattern catch is
-- GREEN. instance Exception Kill where toException =
-- asyncExceptionToException must wrap SomeAsyncException so
-- Warp isAsyncException / fromException (toException e) is True.
-- catch + try of a non-IOException is a separate leftover
-- (IhcException: Boom — see Unsupported/io_catch_typed_handler).
-- Custom ADT. No Exception-type name list.
import Control.Exception

data Boom = Boom String deriving Show
instance Exception Boom

data Kill = Kill deriving Show
instance Exception Kill where
    toException = asyncExceptionToException
    fromException = asyncExceptionFromException

isAsync :: Exception e => e -> Bool
isAsync e =
    case fromException (toException e) of
        Just (SomeAsyncException _) -> True
        Nothing -> False

main :: IO ()
main = do
    print (isAsync Kill)
    print (isAsync ThreadKilled)
    print (isAsync (Boom "sync"))
