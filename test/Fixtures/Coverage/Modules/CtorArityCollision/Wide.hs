module Modules.CtorArityCollision.Wide (wideName) where

-- An arity-2 homonym of the entry module's `Counter` (mirrors
-- network-control's `data Counter = Counter Int UnixTime`).  It is
-- present only to poison the interpreter's bare-name constructor
-- registry; it is not exported.
data Counter = Counter Int Int

wideName :: String
wideName = "wide"
