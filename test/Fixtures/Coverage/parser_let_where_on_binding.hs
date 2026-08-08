-- `where` clause on a binding inside expression-level layout `let`, before `in`.
-- Seen in: conduit-1.3.6.1 Data/Conduit/Internal/Conduit.hs
--   (fuseReturnLeftovers / connectResumeConduit / transPipe):
--     let
--       goLeft rp rc left =
--           case left of
--               ...
--         where
--           recurse = goLeft rp rc
--       in goRight ...
-- Pre-fix: expected `in`; saw TkWhere
--
-- The existing parser_let_where.hs exercises do-let (parseDoLet already
-- had attachDoLetWhere).  This fixture hits expression-level parseLet.

main = do
    print (let
        go n =
            case n of
                0 -> base
                k -> recurse (k - 1)
          where
            base = 42
            recurse = go
        in go 3)
    -- Two bindings, each with its own where, then `in` (conduit shape).
    print (let
        goRight x =
            case x of
                0 -> leafR
                n -> goLeft (n - 1)
          where
            leafR = 1
        goLeft x =
            case x of
                0 -> leafL
                n -> goRight (n - 1)
          where
            leafL = 2
        in goRight 4)
