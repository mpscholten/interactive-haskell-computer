{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- Regression: the {-# LANGUAGE AllowAmbiguousTypes #-} pragma parses
-- cleanly and is a no-op for ihc.  Since ihc is optimistic about types
-- and does not enforce ambiguity checks at runtime, the pragma needs
-- no semantic handling -- the lexer silently skips it and the parser
-- never sees it.  The actual mechanism the pragma unlocks in GHC
-- (signatures whose type variables only appear under a class
-- constraint, picked by TypeApplications at use sites) already works
-- in ihc via value-level TypeApplications (commit 12b01fe) and
-- symbolVal / natVal recovery (commit 10f7f72).  IHP uses this
-- combination in 55+ files.
foo :: forall a. Show a => Int
foo = 42  -- 'a' never appears in the return type

main = print (foo @Int)
