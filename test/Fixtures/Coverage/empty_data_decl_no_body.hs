-- Regression: an empty data declaration (no constructors, no '=' / no
-- 'where') must not absorb the *next* top-level binding's '=' as if it
-- were the data body.
--
-- The trigger in real code is GHC.Internal.MVar:
--
--    data MVar a = MVar (MVar# RealWorld a)
--    ...
--    data PrimMVar
--
--    newStablePtrPrimMVar :: MVar a -> IO (StablePtr PrimMVar)
--    newStablePtrPrimMVar (MVar m) = IO $ \s0 -> ...
--
-- Before the fix, 'scanDataDecls.peekEqOrWhere' walked from
-- @data PrimMVar@ until the first '=' anywhere downstream — which was
-- the '=' in @newStablePtrPrimMVar (MVar m) = IO $ \s0 -> ...@.  It
-- then treated everything after that '=' as constructor declarations
-- of @PrimMVar@, registering @IO@ (and others) as a ctor of
-- @PrimMVar@ in 'lmDataReg'.
--
-- The ctor → type-name hook installed for 'typeTagOf' subsequently
-- mapped every @VCon "IO" _@ runtime value to type @"PrimMVar"@,
-- which broke Functor IO dispatch end-to-end:
--
--    fmap: no Functor instance registered for type `PrimMVar`
--
-- Fix: 'peekEqOrWhere' returns 'TkEof' the moment it sees any
-- column-1 non-trivia token, so an empty 'data T' decl is treated as
-- "no constructors" and its registry footprint is empty.
--
-- The shape below mirrors the failure precisely: an empty data decl
-- followed immediately by a top-level binding whose RHS uses a
-- ConId.  Without the fix, our scanner would register that ConId as
-- a constructor of the empty data, then class dispatch on values of
-- the second type would key on the first type's name.
data Empty

data Wrapper = Wrap Int

-- The function below has '= Wrap' in its body.  Pre-fix scanner sees
-- this '=' as if it were 'data Empty = Wrap', registers @Wrap@ as a
-- constructor of @Empty@.
unwrap :: Wrapper -> Int
unwrap (Wrap n) = n

main :: IO ()
main = print (unwrap (Wrap 42))
