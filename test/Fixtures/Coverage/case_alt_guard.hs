-- Case alternative with guards.  A failing guard must fall through to
-- the next alt, so `classify 0` picks the `_` branch even though the
-- `Just` pattern matches.
classify mv = case mv of
    Just n | n > 0 -> 1
           | n < 0 -> -1
    _              -> 0

main = do
    print (classify (Just 5))
    print (classify (Just 0))
    print (classify (Just (-3)))
    print (classify Nothing)
