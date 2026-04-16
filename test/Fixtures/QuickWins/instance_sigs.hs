-- InstanceSigs: type signatures inside an instance body are silently
-- skipped (GHC2021 implies InstanceSigs). We only check that parsing
-- succeeds here — the result is the method's return value via operator
-- dispatch, which is known to work on master.

data Priority = Low | Medium | High

instance Eq Priority where
    (==) :: Priority -> Priority -> Bool
    (==) Low    Low    = True
    (==) Medium Medium = True
    (==) High   High   = True
    (==) _      _      = False

main = do
    print (Low == Low)
    print (Low == High)
