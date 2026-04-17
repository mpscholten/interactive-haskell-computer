-- Value-level TypeApplications: @T in expression position.
-- ihc parses the type argument into an ETyApp AST node and the
-- evaluator treats it as a pass-through. The visible program behaviour
-- matches the same code with the @T erased.
import Data.Proxy

id' x = x

main = do
    -- Proxy @"Symbol" — literal Symbol type (IHP #fieldName usage)
    let p = Proxy @"email"
    print p
    -- Plain @Int on a user-defined identity
    print (id' @Int 42)
