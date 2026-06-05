-- | Mirrors @GHC.Event.KQueue@: a record whose field selector @filter@
-- collides with @Prelude.filter@. KQueue defines
-- @data Event = KEvent { ident :: ..., filter :: !Filter, ... }@ and uses the
-- @filter@ accessor internally. The selector must stay scoped to this module —
-- importers that never bring it into scope must still see @Prelude.filter@.
module EventBackend (KEvent (KEvent), eventFilter) where

data KEvent = KEvent { ident :: Int, filter :: Int }

-- Internal use of the record accessor, exactly like KQueue's
-- @f (fromIntegral (ident e)) (toEvent (filter e))@.
eventFilter :: KEvent -> Int
eventFilter e = filter e
