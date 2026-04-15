-- | A tiny per-binding item list — an ephemeral intermediate between
-- parse and emit. Not an IR in the persistent sense; lives only for
-- one compile session.
module IHC.IR
    ( Item(..)
    , Binding(..)
    , sizeOfItem
    , sizeOfItems
    , prologueBytes
    , epilogueBytes
    , bindingBytes
    , frameSize
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)

import IHC.Encode (loadInt64, Cond)

data Item
    = IPushX0                       -- spill accumulator to stack
    | IPopX1                        -- pop previous accumulator into x1
    | ILitInt   !Int64              -- materialize Int literal into x0
    | ILitStr   !ByteString         -- materialize (CString) pointer for a
                                    --   string literal into x0
    | IAddX1X0                      -- x0 = x1 + x0
    | ISubX1X0                      -- x0 = x1 - x0
    | IMulX1X0                      -- x0 = x1 * x0
    | ICmp      !Cond               -- x0 = (x1 `cond` x0) ? 1 : 0 (cmp + cset)
    | IAndX1X0                      -- bitwise AND (boolean && for 0/1 values)
    | IOrX1X0                       -- bitwise OR  (boolean || for 0/1 values)
    | INegX0                        -- x0 = -x0 (unary minus)
    | IArg      !Int                -- load nth parameter (0-indexed) into x0
    | ICall     !ByteString !Int    -- call; arity=N means pop N values from
                                    --   stack into x0..x(N-1), then blr
    | IIfThenElse [Item] [Item] [Item]
    deriving (Eq, Show)

data Binding = Binding
    { bindParams :: ![ByteString]
    , bindItems  :: ![Item]
    } deriving (Eq, Show)

sizeOfItem :: Item -> Int
sizeOfItem = \case
    IPushX0       -> 4
    IPopX1        -> 4
    ILitInt n     -> 4 * length (loadInt64 0 n)
    ILitStr _     -> 16                     -- always 4 movz/movk (full addr)
    IAddX1X0      -> 4
    ISubX1X0      -> 4
    IMulX1X0      -> 4
    ICmp _        -> 8                      -- cmp + cset
    IAndX1X0      -> 4
    IOrX1X0       -> 4
    INegX0        -> 4
    IArg _        -> 4
    ICall _ 0     -> 20                     -- 4 movz/movk + 1 blr
    ICall _ n     -> 4 * n                  --   n ldrs (one per arg)
                   + 4                      -- + 1 add sp
                   + 20                     -- + 4 addr-load + 1 blr
    IIfThenElse c t e ->
        sizeOfItems c
        + 4
        + sizeOfItems t
        + 4
        + sizeOfItems e

sizeOfItems :: [Item] -> Int
sizeOfItems = sum . map sizeOfItem

-- | Frame size in bytes for @arity@ arguments. Aligned to 16.
-- Layout: [saved fp, saved lr, arg0, arg1, arg2, arg3, padding...].
frameSize :: Int -> Int
frameSize arity
    | arity == 0 = 16          -- just fp + lr
    | arity <= 2 = 32          -- fp + lr + 2 arg slots
    | arity <= 4 = 48          -- fp + lr + 4 arg slots
    | otherwise  = error ("IHC.IR.frameSize: arity > 4 not supported yet: "
                          <> show arity)

-- | Prologue: stp + mov, plus one @str@ per argument.
prologueBytes :: [ByteString] -> Int
prologueBytes params = 8 + 4 * length params

-- | Epilogue: ldp (deallocates the whole frame via post-index) + ret.
epilogueBytes :: [ByteString] -> Int
epilogueBytes _ = 8

bindingBytes :: Binding -> Int
bindingBytes b =
    prologueBytes (bindParams b)
    + sizeOfItems (bindItems b)
    + epilogueBytes (bindParams b)
