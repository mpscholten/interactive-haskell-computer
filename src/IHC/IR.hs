-- | A tiny per-binding item list used as an ephemeral intermediate
-- between parse and emit.
--
-- This is /not/ a persistent IR — it exists only as long as a single
-- compile session, never serialized, never optimized over. Its purpose
-- is to let us (a) parse each binding in isolation, (b) discover
-- dependencies transitively, (c) lay out all reachable bindings in the
-- code buffer with known addresses, and then (d) emit each binding in
-- one contiguous shot.
module IHC.IR
    ( Item(..)
    , Binding(..)
    , sizeOfItem
    , sizeOfItems
    , prologueBytes
    , epilogueBytes
    , bindingBytes
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)

import IHC.Encode (loadInt64)

data Item
    = IPushX0                 -- spill accumulator to stack
    | IPopX1                  -- pop previous accumulator into x1
    | ILitInt   !Int64        -- materialize literal into x0
    | IAddX1X0                -- x0 = x1 + x0
    | ISubX1X0                -- x0 = x1 - x0
    | IMulX1X0                -- x0 = x1 * x0
    | IArg                    -- load the first argument from arg-slot into x0
    | ICall     !ByteString   -- call nullary binding, result in x0
    | ICall1    !ByteString   -- call 1-arg binding with arg in x0, result in x0
    deriving (Eq, Show)

-- | A discovered + parsed top-level binding.
data Binding = Binding
    { bindParams :: ![ByteString]  -- ^ zero or one in Phase 1.3
    , bindItems  :: ![Item]
    } deriving (Eq, Show)

sizeOfItem :: Item -> Int
sizeOfItem = \case
    IPushX0       -> 4
    IPopX1        -> 4
    ILitInt n     -> 4 * length (loadInt64 0 n)
    IAddX1X0      -> 4
    ISubX1X0      -> 4
    IMulX1X0      -> 4
    IArg          -> 4
    ICall _       -> 20           -- 4 movz/movk + 1 blr
    ICall1 _      -> 20           -- same shape; arg already in x0 at the call site

sizeOfItems :: [Item] -> Int
sizeOfItems = sum . map sizeOfItem

-- | Prologue size depends on arity. Zero-arg uses the 16-byte frame
-- (stp+mov); one-arg uses the 32-byte frame (stp+mov+str).
prologueBytes :: [ByteString] -> Int
prologueBytes [] = 8           -- stp + mov
prologueBytes _  = 12          -- stp + mov + str

-- | Epilogue size matches the prologue's frame choice.
epilogueBytes :: [ByteString] -> Int
epilogueBytes [] = 8           -- ldp + ret
epilogueBytes _  = 8           -- ldp + ret (ldp's post-index deallocates the whole 32)

bindingBytes :: Binding -> Int
bindingBytes b =
    prologueBytes (bindParams b)
    + sizeOfItems (bindItems b)
    + epilogueBytes (bindParams b)
