-- | Mid-@ operators (^@.. / ^@. style).  '@' is not isOpChar so the
-- lexer splits them; peekOp recombines for expression parse (FieldTH
-- residual).  Prefix-form definition @(^@..) xs f = ...@ is the
-- reliable binding shape; infix expression uses still hit the recombine.

(^@..) :: [a] -> (a -> b) -> [b]
(^@..) xs f = map f xs

(^@.) :: Maybe a -> (a -> b) -> Maybe b
(^@.) m f = fmap f m

main :: IO ()
main = do
  print ([1, 2, 3] ^@.. (+10))
  print ((Just 5) ^@. (*2))
  print [ (n, length ts, (\(a, b) -> (b, a)) <$> ts ^@.. id)
        | (n, ts) <- [("a", [(1,"x"), (2,"y")])]
        ]
