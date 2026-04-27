-- List comprehension with a multi-binding `let` qualifier, as used
-- by IHP's RouterSupport.routeMatchParser's actionMatchMap.  The
-- three bindings (a, b, sum) are at the same column after `let`.
main = print (sum pairs)
  where
    pairs =
        [ total
        | x <- [1,2,3]
        , let a = x * 10
              b = x + 1
              total = a + b
        ]
