-- | Forward-declaration for 'IHCRuntime' so 'IHC.Val' can reference it
-- from the 'Closure' constructor without pulling in the real
-- 'IHC.Runtime' module — which would cycle back through 'IHC.Classes'
-- / 'IHC.Scan' / etc.
--
-- The constructor of 'IHCRuntime' is not exposed here; code that
-- actually needs to create or project fields imports 'IHC.Runtime'
-- directly.
module IHC.Runtime (IHCRuntime) where

data IHCRuntime
