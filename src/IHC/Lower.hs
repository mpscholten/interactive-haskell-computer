-- | C.2.2 — read-only lowering pass from 'IHC.AST.Expr' to
-- 'IHC.Core.Core'.
--
-- This slice ships the *structural* lowering: for every 'Expr'
-- constructor, produce the matching 'Core' constructor.  Type
-- annotations are placeholder ('placeholderType') because the
-- elaborator that supplies real types isn't yet wired into the
-- lowering driver.  That wiring lands in C.2.3, alongside the
-- dispatcher swap.  The point of this slice is to:
--
--   1. Prove the structural mapping is faithful — every 'Expr'
--      constructor we currently emit has a 'Core' analogue.
--   2. Surface the small number of nodes that genuinely need type
--      information ('ETypedMethod', 'ETyApp', record-syntax
--      desugaring) so C.2.3 has a concrete worklist.
--   3. Give downstream slices (C.1 GADT refinement, B.5 quantified
--      constraints) a stable function signature to call into.
--
-- Out of scope at this slice:
--
--   * Real types: every node gets 'placeholderType'; consumers must
--     not rely on type annotations being meaningful yet.
--   * Class-method dispatch as 'CDictApp': we lower 'EVar' that
--     names a class method to 'CVar' for now and document that
--     C.2.3 will retarget those sites once the elaborator runs.
--   * Tick / SourceSpan: 'CTick' is reserved; we never emit it.
--   * Splice / quote / quasi-quoter: those run before lowering, so
--     'ESplice' / 'EQuote' / 'EQuasiQuote' are explicit errors —
--     hitting them means the elaborator failed to expand them.
--
-- The function signature is total — every well-formed 'Expr'
-- lowers to *some* 'Core' value, even if the type is a placeholder.
-- Genuinely unrepresentable shapes ('EGuardFail' before guard
-- desugaring runs, splice nodes that survived expansion) raise.
module IHC.Lower
    ( lower
    , lowerStmt
    , placeholderType
    ) where

import qualified Data.ByteString.Char8 as BC

import IHC.AST  (Alt(..), Expr(..), Pat(..), Stmt(..))
import qualified IHC.AST as AST
import IHC.Core
import IHC.TypeAST (Type(..))

-- | The placeholder type used for every 'Core' node produced by this
-- pass.  C.2.3 will replace each occurrence with the real type
-- supplied by the elaborator.  Distinct sentinel so a downstream
-- consumer that genuinely depended on the type can detect that
-- it's looking at lowering output, not elaborator output.
placeholderType :: Type
placeholderType = TyCon (BC.pack "<lower/unknown>")

-- | Lower an 'Expr' to a 'Core' value, using 'placeholderType' for
-- every annotation.  Total on the parser's output; raises on shapes
-- that should have been expanded before lowering (TH splices,
-- guard-failure sentinels reaching code generation, etc.).
lower :: Expr -> Core
lower e = case e of
    EVar n             -> CVar n placeholderType
    ELit lit           -> CLit lit placeholderType

    EApp f x           -> CApp (lower f) (lower x)

    -- 'ELam' in 'IHC.AST' binds a single 'Name' (not a 'Pat'); the
    -- multi-arg / pattern-bind cases were already desugared by the
    -- parser into nested ELam + ECase.  Reuse 'PVar' so 'CLam' has
    -- a pattern even when no pattern existed at source level.
    ELam n body        -> CLam (asPVar n) placeholderType (lower body)

    -- Recursive let group.  'IHC.AST.Bind' is @(Name, Expr)@; we
    -- lift each binding into a 'CBind' with a placeholder type.
    ELet binds body    ->
        let binds' = [ (n, placeholderType, lower rhs) | (n, rhs) <- binds ]
        in CLet binds' (lower body)

    ECase scrut alts   ->
        let alts' = [ CAlt p placeholderType (lower body)
                    | Alt p body <- alts
                    ]
        in CCase (lower scrut) placeholderType alts'

    EIf c t f          ->
        -- Lower @if c then t else f@ to @case c of True -> t; _ -> f@,
        -- matching the shape the existing eval produces internally.
        let alts' =
                [ CAlt (asPCon (BC.pack "True")  []) placeholderType (lower t)
                , CAlt (asPCon (BC.pack "False") []) placeholderType (lower f)
                ]
        in CCase (lower c) placeholderType alts'

    EDo stmts          ->
        -- Defensive: 'IHC.Parser' typically eliminates 'EDo' at parse
        -- time in favour of @>>=@/@>>@/lambda chains.  When it
        -- doesn't (single-stmt fallback paths), lower each statement.
        case stmts of
            [SExpr inner]      -> lower inner
            [SBind _n inner]   -> lower inner
            [SBangBind _n inner] -> lower inner
            _ -> error
                ( "IHC.Lower.lower: EDo with " <> show (length stmts)
                  <> " statements should have been desugared by the parser" )

    ENeg inner         ->
        -- 'negate' is a regular function.  Lower @-x@ to @negate x@.
        CApp (CVar (BC.pack "negate") placeholderType) (lower inner)

    ETuple es          ->
        -- Curried tuple constructor.  E.g. @(a, b)@ lowers to
        -- @(,) a b@.  The tuple-constructor name encodes the arity:
        -- @(,)@ for 2-tuples, @(,,)@ for 3-tuples, etc.
        let arity   = length es
            ctorBs  = BC.pack ("(" <> replicate (arity - 1) ',' <> ")")
            ctorVal = CVar ctorBs placeholderType
        in foldl CApp ctorVal (map lower es)

    -- Record / type-application / implicit-param nodes: lowering is
    -- structural pass-through; the elaborator runs before lowering
    -- and rewrites these into ordinary 'EApp' chains for the cases
    -- we currently support.  When we hit them at lowering time it
    -- means the elaborator hasn't yet wired this slice through —
    -- defer to C.2.3 with a clear error.
    ERecordCon n _      -> error
        ( "IHC.Lower.lower: ERecordCon " <> BC.unpack n
          <> " — record-construction lowering arrives in C.2.3" )
    ERecordWild n       -> error
        ( "IHC.Lower.lower: ERecordWild " <> BC.unpack n
          <> " — RecordWildCards lowering arrives in C.2.3" )
    ERecordUpdate _ _   -> error
        "IHC.Lower.lower: ERecordUpdate — record-update lowering arrives in C.2.3"

    EImplicitRef n      -> CVar (BC.cons '?' n) placeholderType
    EImplicitLet binds body ->
        let binds' = [ (BC.cons '?' n, placeholderType, lower rhs)
                     | (n, rhs) <- binds
                     ]
        in CLet binds' (lower body)

    -- TH-related nodes: should have been expanded before lowering.
    ESplice _    -> error "IHC.Lower.lower: ESplice survived to lowering"
    EQuote  _    -> error "IHC.Lower.lower: EQuote survived to lowering"
    EQuasiQuote _ _ -> error
        "IHC.Lower.lower: EQuasiQuote survived to lowering"

    -- Phase 3.5 OverloadedLabels: a label is a 'CVar' with a
    -- distinguishing prefix.  The elaborator (eventually) rewrites
    -- this through 'IsLabel'; today it's an opaque constant.
    ELabel n            -> CVar (BC.cons '#' n) placeholderType

    -- Value-level @e \@T@ TypeApplications.  C.2.3 will retarget
    -- this to 'CDictApp' once the elaborator supplies the resolved
    -- 'Dict'; today, lower as a plain 'CVar'/'CApp' — semantically
    -- correct but doesn't yet exercise the dictionary path.
    ETyApp inner _      -> lower inner
    ELocalSig _ inner   -> lower inner

    -- Class-method site already resolved by the elaborator to a
    -- specific instance.  This is where C.2.3 will emit
    -- 'CDictApp'; today, we keep it as a 'CVar' with the method
    -- name so the lowered Core is still well-shaped.
    ETypedMethod _cls method _instTag ->
        CVar method placeholderType

    -- Constraint evidence is evaluator metadata.  Until Core grows
    -- dictionary applications for it, lower the wrapped value normally.
    EConstrainedValue inner _constraints ->
        lower inner

    -- Guard sentinel — should have been desugared to nested case
    -- alternatives by the parser before reaching the lowering pass.
    EGuardFail -> error "IHC.Lower.lower: EGuardFail survived to lowering"

-- | Lower a 'Stmt' (used by 'EDo' fallback paths above).  Currently
-- a thin shim — the parser eliminates do-blocks at parse time, so
-- 'lowerStmt' is provided for completeness rather than active use.
lowerStmt :: Stmt -> Core
lowerStmt (SExpr e)        = lower e
lowerStmt (SBind _ e)      = lower e
lowerStmt (SBangBind _ e)  = lower e
lowerStmt (SLet binds)     =
    let binds' = [ (n, placeholderType, lower rhs) | (n, rhs) <- binds ]
    in CLet binds' (CVar (BC.pack "()") placeholderType)
lowerStmt (SImplicitLet binds) =
    let binds' = [ (BC.cons '?' n, placeholderType, lower rhs)
                 | (n, rhs) <- binds
                 ]
    in CLet binds' (CVar (BC.pack "()") placeholderType)

--------------------------------------------------------------------------------
-- Internal pattern helpers
--------------------------------------------------------------------------------

-- These build 'IHC.AST.Pat' values for nodes where the lowering
-- needs a pattern but the source 'Expr' didn't supply one ('ELam'
-- only stores a 'Name', 'EIf' has no scrutinee pattern, etc.).
asPVar :: BC.ByteString -> AST.Pat
asPVar = PVar

asPCon :: BC.ByteString -> [AST.Pat] -> AST.Pat
asPCon = PCon
