-- Regression: scanDataDecls skipped an existential data declaration whose
-- context is a TUPLE constraint, e.g.
--
--   data H = forall dev. (C1 dev, C2 dev) => H { hType :: Int, ... }
--
-- The constraint tuple begins with '(' which collectCtors treated as the
-- start of an infix constructor, so the record's fields were never
-- registered and the field accessor (hType) showed up as an "unbound
-- variable". A SINGLE-constraint existential (`forall a. C a => ...`)
-- worked because that path starts with a ConId and is constraint-checked.
--
-- This is the shape of warp's GHC.Internal.IO.Handle.Types.Handle__
--   forall dev enc dec. (RawIO dev, IODevice dev, BufferedIO dev, Typeable dev)
--     => Handle__ { haDevice :: !dev, haType :: HandleType, ... }
-- whose `haType` accessor was an unbound variable on warp's request-path
-- error-reporting code (Data.Text.IO via wantWritableHandle).
{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}

class RawIO a
class IODev a
instance RawIO Int
instance IODev Int

data H = forall dev . (RawIO dev, IODev dev, Show dev) =>
    H { hDevice :: !dev
      , hType   :: Int
      }

main :: IO ()
main = print (hType (H { hDevice = (0 :: Int), hType = 42 }))
