-- Pragmas inside an instance body (INLINE, SPECIALIZE) are already
-- consumed as trivia by the lexer, so they coexist with InstanceSigs
-- type signatures without disturbing scanning.

data Priority = Low | High

instance Eq Priority where
    {-# INLINE (==) #-}
    (==) :: Priority -> Priority -> Bool
    (==) Low  Low  = True
    (==) High High = True
    {-# SPECIALIZE (==) :: Priority -> Priority -> Bool #-}
    (==) _    _    = False

main = do
    print (Low  == Low)
    print (Low  == High)
    print (High == High)
