-- | Typed post-rename intermediate representation ("ihc-Core").
--
-- C.2.1 — datatype only.  No constructors of 'Core' are produced
-- anywhere in the codebase yet; the lowering pass that takes
-- 'IHC.AST.Expr' and produces 'Core' lives in @IHC.Lower@ (C.2.2)
-- and the dispatcher swap that consumes it lives in @IHC.Eval@ (C.2.3).
-- This module ships the type so subsequent slices have a stable
-- shape to target.
--
-- Why a typed IR at all?  The B.3 (overlap), B.4 (functional
-- dependencies), B.5 (quantified constraints), and C.1 (GADT
-- refinement) deferrals all bottom out on the same thing: the
-- existing 'IHC.AST.Expr' loses every shred of type information at
-- parse time, so the runtime tag-keyed dispatcher in 'IHC.Eval' has
-- nothing to query when an instance lookup needs to discriminate by
-- type structure (@[Char]@ vs @[Int]@) or by quantified context.
-- Core threads a 'Type' annotation on every node and represents
-- class-dictionary application as an explicit 'CDictApp', so:
--
-- * Specificity-aware overlap (B.3) becomes a lookup against the
--   target type rather than its eroded tag.
-- * FunDep improvement (B.4) can run in the elaborator that produces
--   Core: knowing the type at every node lets the solver propagate
--   @b ~ b'@ from two pending @C T b@ / @C T b'@ constraints.
-- * Quantified constraints (B.5) attach a 'CDictLam' at the
--   binding site and discharge it via 'CDictApp' at the use site.
-- * GADT refinement (C.1) is the existing 'CCast' node carrying a
--   'Coercion' that the type checker minted at the pattern-match.
--
-- The migration is staged:
--
--   C.2.1 (this slice): datatype + module exports.  No producer, no
--                       consumer.  Build-only validation.
--   C.2.2 (next):       a 'Lower' pass that takes elaborated 'Expr'
--                       and emits 'Core'.  Run alongside the
--                       existing eval and assert via a differential
--                       harness that the lowered shape would
--                       dispatch identically.
--   C.2.3 (later):      'IHC.Eval' switches to consuming 'Core' for
--                       class-method dispatch; the runtime
--                       'VClassMethod' tag-keyed path is deprecated.
--
-- Notes on shape:
--
-- * 'Core' reuses 'IHC.AST.Pat' rather than introducing a separate
--   pattern AST.  Patterns don't carry types in surface Haskell
--   either; type information about a binding lives on the 'CLam' /
--   'CCase' node that introduces it.
-- * 'CLet' carries a list of bindings rather than a single binding
--   so we keep the existing recursive-group semantics; mutually
--   recursive lets must share a slot environment.
-- * 'Tick' is a profiling / debug placeholder.  Keep it minimal —
--   we may extend later.
module IHC.Core
    ( Core(..)
    , CAlt(..)
    , CBind
    , Dict(..)
    , DictBinder(..)
    , Coercion(..)
    , Tick(..)
    , coreType
    , coercionTarget
    ) where

import Data.ByteString (ByteString)

import IHC.AST  (Lit, Name, Pat)
import IHC.TypeAST (Pred, Type(..))

--------------------------------------------------------------------------------
-- Core expressions
--------------------------------------------------------------------------------

-- | Typed intermediate representation.  Every node carries (or
-- derives in 'coreType') its type so consumers — dispatcher, codegen,
-- profilers — never need to re-derive types from sub-expressions.
data Core
    = -- | Variable reference, with the binding's monomorphic type
      -- applied at the use site.  E.g. @show \@Int@ is
      -- @CVar "show" (TyArrow (TyCon "Int") (TyCon "String"))@.
      CVar !Name !Type
      -- | Literal with the type chosen for its context.  Numeric
      -- literals from polymorphic source positions arrive here only
      -- after defaulting (A.4) has resolved them.
    | CLit !Lit !Type
      -- | Function application.  The callee's type must agree with
      -- the argument's type at the type checker's instantiation.
      -- The result type is computed by stripping one arrow from the
      -- callee.
    | CApp !Core !Core
      -- | Lambda introducing a value-level binding.  The 'Pat'
      -- describes the surface pattern; the 'Type' is the parameter's
      -- type; the body's type is the lambda's result type.
    | CLam !Pat !Type !Core
      -- | Mutually recursive let.  All bindings share a single
      -- closure environment.
    | CLet ![CBind] !Core
      -- | Case discrimination with an explicit result type.  Stored
      -- so codegen / dispatch don't need to re-infer it from the
      -- alternatives.
    | CCase !Core !Type ![CAlt]
      -- | Explicit class-dictionary application.  The dictionary
      -- argument is a 'Dict' value, computed at the elaborator's
      -- instance lookup.  This is the typed-IR replacement for the
      -- runtime tag-keyed 'VClassMethod' dispatcher.
    | CDictApp !Core !Dict
      -- | Explicit class-dictionary abstraction.  A 'CDictLam'
      -- introduces a dictionary binding at the surface site of a
      -- class context (or quantified constraint, B.5); a matching
      -- 'CDictApp' at the use site supplies the resolved dictionary.
    | CDictLam !DictBinder !Core
      -- | Type refinement (GADT pattern-match, type-family
      -- reduction, newtype wrap/unwrap).  The 'Coercion' is the
      -- proof of the type identity; the cast retypes the result.
      -- See 'Coercion' for the form.
    | CCast !Core !Coercion
      -- | Profiling / debug breadcrumb.  Currently unused; reserved
      -- for SCC-style cost-centre annotations.
    | CTick !Tick !Core
    deriving (Eq, Show)

-- | A case-alternative.  The elaborator pre-desugars guards before
-- lowering, so 'CAlt' carries only a single body.
data CAlt = CAlt
    { caltPat  :: !Pat
    , caltType :: !Type   -- ^ result type of this alternative's body
    , caltBody :: !Core
    } deriving (Eq, Show)

-- | A 'CLet' binding: name + monomorphic type + body.  Recursive
-- groups share a single 'CLet' so the evaluator can knot-tie.
type CBind = (Name, Type, Core)

--------------------------------------------------------------------------------
-- Dictionaries (class-method dispatch)
--------------------------------------------------------------------------------

-- | A class-dictionary value: how the elaborator says "this method
-- comes from instance X."  The runtime materialises 'Dict' as the
-- existing @MethodTable@ ('IHC.Classes.MethodTable') keyed by class
-- name; in C.2.3 the dispatcher will resolve a 'Dict' to a method
-- value directly without going through the @typeTagOf@ tag-string.
data Dict
    = -- | A specific instance: @Dict (className, [type-args])@.
      -- @DInstance "Show" [TyApp (TyCon "Maybe") (TyCon "Int")]@
      -- selects @instance Show (Maybe Int)@.
      DInstance !Name ![Type]
      -- | A superclass projection: given a dictionary for class C
      -- whose declaration says @C => D@, the @D@ dictionary is
      -- @DSuperclass parent superclassName@.  This is what makes
      -- B.1's superclass-relation usable from the dispatcher.
    | DSuperclass !Dict !Name
      -- | A lambda-bound dictionary (B.5 quantified constraint).
      -- The de-Bruijn index is into the enclosing 'CDictLam' chain.
      -- Kept opaque at this slice; consumers in C.2.3 will resolve.
    | DVar !Int
    deriving (Eq, Show)

-- | A dictionary binder introduced by a 'CDictLam'.  Records the
-- predicate the binding satisfies so the runtime can verify when
-- a 'CDictApp' supplies a 'Dict' value.
data DictBinder = DictBinder
    { dbPred  :: !Pred    -- ^ the constraint discharged by this binder
    , dbIndex :: !Int     -- ^ position in the surrounding lambda's binders
    } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Coercions (type refinement)
--------------------------------------------------------------------------------

-- | Type-equality proof attached to a 'CCast'.  The minimum useful
-- shape for GADT refinement (C.1) and newtype wrap/unwrap.  Real
-- coercions in GHC carry a tree of subproofs; we'll grow this as
-- the consumers need it.
data Coercion
    = -- | Reflexivity: @t ~ t@.  No-op cast inserted at points where
      -- a typed annotation needs to be threaded through but no
      -- structural change occurs.
      CoRefl !Type
      -- | GADT refinement: @from ~ to@ from a pattern-match against a
      -- GADT constructor that proves @from = to@.  The body of the
      -- case-alt is retyped from 'from' to 'to'.
    | CoGadt !Type !Type
      -- | Newtype wrap/unwrap: @T ~ Repr T@ for an unspecified
      -- direction.  C.2 will refine this once newtypes get a
      -- representation distinct from 'data'.
    | CoNewtype !Type !Type
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Ticks (debug / profiling)
--------------------------------------------------------------------------------

-- | A profiling / debug breadcrumb.  Today this is just a tag string
-- so we can compile the type without committing to a specific
-- profiling protocol; we'll extend the variant set when a consumer
-- needs more (cost-centre, source-span, breakpoint, etc.).
data Tick
    = TkCostCentre !ByteString
    | TkSourceSpan !ByteString !Int !Int    -- ^ filename, line, column
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | The type of a 'Core' expression.  Walks the structure rather than
-- carrying a separate field on every node so that constructing a
-- 'Core' value can't disagree with its type — the type IS computed
-- from the constructor + sub-expressions.
--
-- For 'CApp' we strip one arrow from the function's type.  Caller
-- contract: the elaborator must have produced well-typed Core; if
-- it didn't, 'coreType' raises rather than silently lying.
coreType :: Core -> Type
coreType (CVar _ t)         = t
coreType (CLit _ t)         = t
coreType (CApp f _)         = stripArrow (coreType f)
coreType (CLam _ argT body) = TyArrow argT (coreType body)
coreType (CLet _ body)      = coreType body
coreType (CCase _ t _)      = t
coreType (CDictApp e _)     = coreType e
coreType (CDictLam _ body)  = coreType body
coreType (CCast _ co)       = coercionTarget co
coreType (CTick _ inner)    = coreType inner

-- | The target type of a 'Coercion' (post-cast).
coercionTarget :: Coercion -> Type
coercionTarget (CoRefl t)        = t
coercionTarget (CoGadt _ to)     = to
coercionTarget (CoNewtype _ to)  = to

-- | Strip one arrow from a function type.  Used by 'coreType' on
-- 'CApp'.  Triggers a clear error rather than a phantom type if
-- the elaborator emitted an ill-typed application.
stripArrow :: Type -> Type
stripArrow (TyArrow _ result) = result
stripArrow t = error
    ("IHC.Core.coreType: CApp on non-arrow type " <> show t
     <> " — elaborator emitted ill-typed Core")
