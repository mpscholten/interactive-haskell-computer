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
    , Stmt(..)
    , Bind
    , Alt(..)
    , Pat(..)
    , Lit(..)
    , Label
    , stripQualifier
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Int (Int64)

type Name  = ByteString
type Label = ByteString

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
    | EDo   ![Stmt]                     -- do-block statements (Phase 2.4)
    | ENeg  !Expr                       -- unary minus
    | ETuple ![Expr]                    -- (a, b, ...) tuple (Phase 2.6)
    | EImplicitRef  !Name              -- ?name reference (Phase 3.6)
    | EImplicitLet  ![(Name, Expr)] !Expr -- let ?x = e in body (Phase 3.6)
    | ERecordCon !Name ![(Name, Expr)] -- Con { f1 = e1, ... } record literal
    | ERecordWild !Name                -- Con {..} — RecordWildCards construction
    | ERecordUpdate !Expr ![(Name, Expr)] -- expr { f1 = e1, ... } record update
    | ESplice  !Expr                   -- $( expr ) TH splice (Phase 2.11)
    | EQuote   !Expr                   -- [| expr |] TH expression bracket (Phase 2.12)
    -- | @[qqName|body|]@ — QuasiQuoter.  Holds the raw bytes of the body
    -- so the evaluator can feed them to @qqName.quoteExp :: String -> Q Exp@
    -- at run time.  Opaque to every other pass (splice expansion, elaboration).
    | EQuasiQuote !Name !ByteString
    | ELabel !Label                     -- #name OverloadedLabels label (Phase 3.5)
    -- | @f \@T@ — value-level TypeApplications. The @Name@ holds the raw
    -- source bytes of the type argument (e.g. @"Int"@, @"Maybe Int"@, @"\"email\""@).
    -- ihc is optimistic about types: the evaluator treats this as a
    -- pass-through on the inner expression. The type argument is retained
    -- as metadata for future @Typeable@ / dictionary-selection use.
    | ETyApp !Expr !Name
    -- | Class method resolved by the on-demand elaborator
    -- ('IHC.Elaborate') to a specific instance.  Three fields:
    -- class name, method name, resolved instance tag.  Evaluator
    -- performs a direct 'lookupInstanceMethod' on the class registry
    -- instead of going through the VClassMethod dispatcher.  Produced
    -- only by elaboration — parsers never emit this node.
    | ETypedMethod !Name !Name !Name
    -- | Internal sentinel used by guarded case alternatives. If an
    -- alternative pattern matches but its guards fail, evaluation should
    -- continue with the next alternative.
    | EGuardFail
    deriving (Eq, Show)

-- | A single statement inside a do-block.
--
--   * 'SExpr' — a bare expression, e.g. @putStrLn "hi"@
--   * 'SBind' — a bind statement @x <- action@
--   * 'SLet'  — @let x = e; y = e2@ inside do (no @in@ — the scope is
--               the remainder of the do-block)
data Stmt
    = SExpr !Expr
    | SBind !Name !Expr
    | SLet  ![Bind]
    | SImplicitLet ![(Name, Expr)]
    deriving (Eq, Show)

type Bind = (Name, Expr)

data Alt = Alt !Pat !Expr
    deriving (Eq, Show)

data Pat
    = PVar  !Name                       -- bind to a name (matches anything)
    | PLit  !Lit                        -- match literal exactly
    | PWild                             -- _, matches anything, no binding
    | PCon  !Name ![Pat]                -- constructor pattern (Phase 2.1+)
    | PAs   !Name !Pat                  -- xs\@(x:_) as-pattern (Phase 2.6)
    | PBang !Pat                        -- !x bang-pattern — we ignore strictness (Phase 2.6)
    | PTuple ![Pat]                     -- (a, b, ...) tuple pattern (Phase 2.6)
    | PRecord !Name ![(Name, Pat)]      -- Con { f1 = p1, f2 = p2, ... } record pattern (NamedFieldPuns)
    | PRecordWild !Name                 -- Con {..} — RecordWildCards pattern
    | PView !Expr !Pat                  -- (f -> p) view pattern (ViewPatterns)
    deriving (Eq, Show)

data Lit
    = LInt    !Int64
    | LFloat  !Double
    | LStr    !ByteString
    | LChar   !Char
    deriving (Eq, Show)

-- | Strip a module qualifier from a name, returning only the final segment.
-- E.g. \"M.Just\" → \"Just\", \"Data.Maybe.Nothing\" → \"Nothing\", \"Just\" → \"Just\".
stripQualifier :: Name -> Name
stripQualifier n = case BC.elemIndexEnd '.' n of
    Nothing -> n
    Just i  -> BC.drop (i + 1) n
