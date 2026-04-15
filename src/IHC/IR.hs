-- | A tiny per-binding item list used as an ephemeral intermediate
-- between parse and emit.
--
-- This is /not/ a persistent IR — it exists only as long as a single
-- compile session, never serialized, never optimized over. Its purpose
-- is to let us (a) parse each binding in isolation, (b) discover
-- dependencies transitively, (c) lay out all reachable bindings in the
-- code buffer with known addresses, and then (d) emit each binding in
-- one contiguous shot. Without it, recursive dependency compilation
-- would interleave emissions and scramble the bump-pointer layout.
--
-- Each item corresponds to a handful of aarch64 instructions; 'sizeOf'
-- gives the emitted byte count so the scheduler can lay bindings out
-- in the buffer before it knows their contents.
module IHC.IR
    ( Item(..)
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
    = IPushX0                    -- spill accumulator to stack
    | IPopX1                     -- pop previous accumulator into x1
    | ILitInt   !Int64           -- materialize literal into x0
    | IAddX1X0                   -- x0 = x1 + x0
    | ISubX1X0                   -- x0 = x1 - x0
    | IMulX1X0                   -- x0 = x1 * x0
    | ICall     !ByteString      -- call another top-level binding, result in x0
    deriving (Eq, Show)

sizeOfItem :: Item -> Int
sizeOfItem = \case
    IPushX0       -> 4
    IPopX1        -> 4
    ILitInt n     -> 4 * length (loadInt64 0 n)
    IAddX1X0      -> 4
    ISubX1X0      -> 4
    IMulX1X0      -> 4
    ICall _       -> 20           -- always 4 movz/movk + 1 blr

sizeOfItems :: [Item] -> Int
sizeOfItems = sum . map sizeOfItem

-- | Prologue is @stp x29,x30,[sp,#-16]! ; mov x29,sp@ — 8 bytes.
prologueBytes :: Int
prologueBytes = 8

-- | Epilogue is @ldp x29,x30,[sp],#16 ; ret@ — 8 bytes.
epilogueBytes :: Int
epilogueBytes = 8

bindingBytes :: [Item] -> Int
bindingBytes items = prologueBytes + sizeOfItems items + epilogueBytes
