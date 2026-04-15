-- | The AST that the parser produces and the evaluator consumes.
--
-- This is the central data structure of Phase 2 — it replaces the
-- Phase-1 'IHC.IR.Item' list. Where Items were a flat sequence of
-- aarch64-instruction-sized actions, Expr is a real abstract syntax
-- tree with the structure needed for laziness, pattern matching, and
-- (eventually) type classes.
--
-- For Phase 2.0 the surface stays the same as Phase-1's final state
-- (Int + String literals, arithmetic, comparisons, if, case-on-Int,
-- let, where, do, multi-arg functions). Subsequent phases extend
-- this module rather than replacing it.
module IHC.AST
    ( Name
    , Expr(..)
    , Bind
    , Alt(..)
    , Pat(..)
    , Lit(..)
    ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)

type Name = ByteString

-- | Source-level expressions. Lazy semantics are encoded in the
-- evaluator, not the AST itself — every sub-Expr is a candidate for
-- being delayed in a thunk.
data Expr
    = EVar  !Name                       -- variable reference
    | ELit  !Lit                        -- literal
    | EApp  !Expr !Expr                 -- function application (curried)
    | ELam  !Name !Expr                 -- lambda \x -> body
    | ELet  ![Bind] !Expr               -- (mutually) recursive let group
    | ECase !Expr ![Alt]                -- pattern match
    | EIf   !Expr !Expr !Expr           -- if-then-else (sugar for case)
    | EDo   ![Expr]                     -- do-block: each Expr is a stmt;
                                        --   later phases add bind-stmts
    | ENeg  !Expr                       -- unary minus
    deriving (Eq, Show)

type Bind = (Name, Expr)

data Alt = Alt !Pat !Expr
    deriving (Eq, Show)

data Pat
    = PVar  !Name                       -- bind to a name (matches anything)
    | PLit  !Lit                        -- match literal exactly
    | PWild                             -- _, matches anything, no binding
    | PCon  !Name ![Pat]                -- constructor pattern (Phase 2.1+)
    deriving (Eq, Show)

data Lit
    = LInt  !Int64
    | LStr  !ByteString
    | LChar !Char
    deriving (Eq, Show)
