module TextConsumer (consume, textResult) where

import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T

-- Deliberately collides with BoolConsumer.consume.  The second argument must
-- be resolved from this module's lexical scope, never the flat global winner.
consume :: Int -> Text -> String
consume _ = T.unpack

textResult :: String
textResult = consume 0 (cs "owner")
