-- Regression: arithmetic-sequence syntax on a derived Enum.
--
-- Parser fixes (IHC.Parser.parseListLit / skipTypeToBinding):
--   * @[x ..]@ desugars to @enumFrom x@ (was a 1-element list).
--   * @[x, y ..]@ desugars to @enumFromThen x y@ (was a 2-element list).
--   * a @:: T@ annotation on the first bound must not swallow the @..@:
--     @[minBound :: M .. maxBound]@ (http-types' methodArray element list)
--     previously parsed as @[minBound :: (M .. maxBound)]@ → @[GET]@.
--
-- Deriving fix (registerEnum): enumFromTo / enumFrom / enumFromThen /
-- enumFromThenTo are synthesized from constructor order.  The stock
-- @enumFromTo@ default (@map toEnum [fromEnum x .. fromEnum y]@) needs a
-- type hint on @toEnum@ to pick the instance; under deferred typing it
-- defaulted to the Int identity and returned Ints (e.g. @enumFromTo GET
-- DELETE@ gave @[0,1,2,3,4]@).
data M = GET | POST | HEAD | PUT | DELETE
    deriving (Show, Eq, Ord, Enum, Bounded)

main :: IO ()
main = do
    print [minBound :: M .. maxBound]        -- [GET,POST,HEAD,PUT,DELETE]
    print [GET ..]                           -- enumFrom
    print [GET, HEAD ..]                      -- enumFromThen (step 2)
    print [GET .. PUT]                        -- enumFromTo
    print (enumFromThenTo GET HEAD DELETE)    -- [GET,HEAD,DELETE]
