-- Facade: re-exports empty from Internal.  No col-1 binding here.
-- Mirrors Data.Set (empty) vs Data.Set.Internal (empty = Tip).
module Modules.FacadeEmpty.Facade (empty, isEmpty, TipBin(..)) where

import Modules.FacadeEmpty.Internal
