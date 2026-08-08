-- Gap: hanging multi-binding layout let — first bind on `let` line, next binds
-- at the same layout column. (Previously mis-indented in the RED fixture.)
-- Ref: Hs2010LexLayout multi-binding layout let; conduit/hasql hanging lets.
main = print (let x = 1
                  y = 2
              in x + y)
