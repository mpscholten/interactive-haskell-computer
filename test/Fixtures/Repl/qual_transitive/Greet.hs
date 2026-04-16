module Greet (greet, parts) where

import qualified Helper as H

-- 'parts' uses H-qualified functions internally.  This module's body
-- references `H.join` and `H.surround`, which the scheduler must be
-- able to resolve by consulting THIS module's imports (not the REPL's).
parts :: String -> String -> String
parts a b = H.join " - " [a, b]

greet :: String -> String
greet name = H.surround "(" ")" (parts "hi" name)
