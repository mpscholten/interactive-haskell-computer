-- Multi-clause local infix operator inside layout `let`.
-- Seen in: conduit-1.3.6.1 Data/Conduit/Internal/Conduit.hs zipSinks:
--   let
--     Leftover _ i >< _            = absurd i
--     _            >< Leftover _ i = absurd i
--     Done x       >< Done y       = rest (x, y)
--     ...
--     in injectLeftovers (x0 Done) >< injectLeftovers (y0 Done)
-- Pre-fix: expected `=` in pattern let-binding; saw TkSymOp "><"
--
-- Reduced to a small ADT + local infix `+:` with constructor patterns
-- on both sides of the operator.

data Pipe
    = Leftover Int
    | Done Int
    | Other

main = do
    print (let
        Leftover i +: _          = i
        _          +: Leftover j = j
        Done x     +: Done y     = x + y
        Other      +: r          = (-1)
        r          +: Other      = (-2)
        in Leftover 10 +: Done 5)
    print (let
        Leftover i +: _          = i
        _          +: Leftover j = j
        Done x     +: Done y     = x + y
        Other      +: r          = (-1)
        r          +: Other      = (-2)
        in Done 3 +: Done 4)
    print (let
        Leftover i +: _          = i
        _          +: Leftover j = j
        Done x     +: Done y     = x + y
        Other      +: r          = (-1)
        r          +: Other      = (-2)
        in Other +: Done 9)
