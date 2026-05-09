# Plan: full source-loaded `ghc-bignum` `Integer` end-to-end

## Why

IHC currently uses its own `VInteger` (host `Integer`) runtime type for
arbitrary-precision integers, completely decoupled from
`ghc-bignum`'s `data Integer = IS !Int# | IP !BigNat# | IN !BigNat#`.
Any source-loaded code that does Integer arithmetic crosses the
boundary at every operation, and IHC currently lacks the bridges.

This is the actual blocker for several quality-of-life graduations
(`floor` / `ceiling` / `round` / `truncate` / `fromIntegral` to
arbitrary types) and on the path to the bytestring north-star
(`Data.ByteString.Lazy from source`).

## What we already have (foundation laid 2026-05-09)

| PR | What it shipped | Where it fits |
|----|-----------------|---------------|
| [#131](https://github.com/mpscholten/interactive-haskell-computer/pull/131) | `decodeDouble_Int64#` primop + 17 Int64# / Int# bitwise primop aliases | Phase 2 below — first 17 of ~124 primops |
| [#134](https://github.com/mpscholten/interactive-haskell-computer/pull/134) | `NegativeLiterals` parser support; `WordSize.h` macros pre-defined in `defaultCppContext`; `ENeg` handles `VInteger`; `asInt64` helper in `IHC.Builtins` | Cross-cutting plumbing — needed wherever ghc-bignum source uses `-0x...` literals or out-of-Int64 values |
| [#136](https://github.com/mpscholten/interactive-haskell-computer/pull/136) | `matchPat` cases for `PCon "IS"/"IP"/"IN"` against `VInt`/`VInteger`; `coerceInt64` in `IHC.Builtins` | Phase 4 below — pattern-direction bridge |

These three PRs are on master and won't be lost. Today's session
also tracked the carve-outs in inline comments next to the relevant
shims (`floor` / `ceiling` / `round` / `truncate` in
`src/IHC/Builtins.hs` around line 290).

## What's left

### Phase 1 — `BigNat#` runtime representation

**Single PR.** Choose how IHC stores `BigNat#` at runtime.

Recommendation: wrap host `Natural` (unsigned arbitrary precision)
in `VPrimObj (PrimBigNat <Natural>)`. Adds one constructor to
`PrimVal` in `src/IHC/Val.hs`, plus the obvious `Eq`/`Show`/`==`
hooks. Mirrors the pattern of `PrimByteArray`, `PrimIORef`, etc.

Smoke test: a primop that returns a constant `VPrimObj
(PrimBigNat 12345)`, plus a Coverage fixture that pattern-matches
`IP bn` against it via the matchPat bridge from #136.

### Phase 2 — `BigNat#` primop suite

**5–10 small PRs over a week.** ~124 primops to implement. Each
is a thin wrapper around host `Natural` arithmetic. Group by
shape:

| Tranche | Primops | Estimated PR size |
|---------|---------|-------------------|
| 2.A — comparison | `bigNatCompare#`, `bigNatEq#`, `bigNatLt#`, `bigNatGt#`, `bigNatLe#`, `bigNatGe#`, `bigNatNe#`, `bigNatIsZero#`, `bigNatIsOne#`, `bigNatSize#` | ~10 primops, ~150 LoC |
| 2.B — arithmetic | `bigNatAdd`, `bigNatMul`, `bigNatSub`, `bigNatQuot`, `bigNatRem`, `bigNatQuotRem`, `bigNatPow#`, `bigNatGcd`, `bigNatLcm` | ~15 primops, ~200 LoC |
| 2.C — bit ops | `bigNatAnd`, `bigNatOr`, `bigNatXor`, `bigNatComplement`, `bigNatShiftL`, `bigNatShiftR`, `bigNatPopCount#`, `bigNatTestBit#`, `bigNatBit#` | ~15 primops, ~200 LoC |
| 2.D — conversions | `bigNatFromWord#`, `bigNatToWord#`, `bigNatFromInt#`, `bigNatToInt#`, `bigNatFromWord64#`, `bigNatToWord64#`, `bigNatFromInteger#`, `integerToBigNatClamp#`, `bigNatEncodeDouble#` | ~25 primops, ~300 LoC |
| 2.E — I/O / show | `bigNatShow`, `bigNatRead`, `bigNatToHexString` | ~5 primops, ~100 LoC |
| 2.F — long tail | misc helpers + zero/one constants | ~50 primops |

Each tranche follows the precedent in
[`#125`](https://github.com/mpscholten/interactive-haskell-computer/pull/125)
(arithmetic shim graduations): one PR, registers the primops,
adds Coverage fixtures.

Reference for primop signatures:
[`~/.cache/ihc/sources/ghc-bignum-1.3/src/GHC/Num/BigNat.hs`](file:///Users/marc/.cache/ihc/sources/ghc-bignum-1.3/src/GHC/Num/BigNat.hs).

### Phase 3 — Construct-direction policy for IS / IP / IN

**Single PR.** When source code does `IS x`, IHC currently
builds `VCon "IS" [thunk]`. With Phase 1+2 in place, decide:

- **Option A** (transparent): collapse `VCon "IS" [VInt x]` to
  `VInt x` at construction time. Symmetric with the matchPat
  bridge from #136 — runtime never sees `VCon "IS"`. Cleanest
  but requires data-constructor application to special-case
  `IS` / `IP` / `IN`.
- **Option B** (wrap, unwrap-on-use): leave `VCon` as-is at
  construction; primops always unwrap via `coerceInt64`. Less
  invasive but every primop carries the unwrap cost.

Recommendation: A. The transparent collapse mirrors how
`F#` / `D#` / `I#` are already handled.

### Phase 4 — `Num Integer` / `Integral Integer` class instance registration

**1–2 small PRs.** Source-loaded `Num Integer` exists in
`GHC.Num.Integer`; once Phase 1+2 land, the source-loaded methods
should "just work" via the source-load path. But class dispatch
needs IHC to recognize the `Integer` type tag and route there.

Today's float→Int graduation hit `<<ihc-method-placeholder>>`
exactly because the `Num Integer` instance isn't registered with
the dispatcher. Two paths:

- Source-load `instance Num Integer` from
  `GHC.Num.Integer` (would work if Phase 1+2 are in)
- Register a builtin shim that uses `coerceInt64` + native
  arithmetic for in-Int64 values, falls through to source for
  big values

Likely both: shim for the fast path (Int-range), source for
the slow path (BigNat).

### Phase 5 — Migration / cleanup

**Multi-PR.** Once Phases 1–4 land:

- Remove `VInteger` if `VPrimObj PrimBigNat` covers everything,
  OR keep `VInteger` as the in-Int64 representation and use
  `PrimBigNat` only for out-of-range. Pick one based on
  measurement.
- Graduate the float→Int shims (`floor` / `ceiling` / `round` /
  `truncate`) — the original motivation. Should "just work"
  once Phases 1–4 are in.
- Graduate `fromIntegral` (still shimmed) — same chain.
- Remove the inline carve-out comments that today's session
  added at `src/IHC/Builtins.hs` around line 290.

## Total estimated scope

**1–2 weeks of focused work**, ~10 PRs. Each PR follows the
project's reproduce → fixture → fix → verify loop with at
least one Coverage fixture per phase.

## Critical files

- `src/IHC/Val.hs` — `Val` / `PrimVal` (Phase 1)
- `src/IHC/Builtins.hs` — primop registry + `coerceInt64`
  (Phase 2, lots of edits)
- `src/IHC/Eval.hs` — `matchPat` (already done in #136),
  `apply` (Phase 3)
- `src/IHC/Classes.hs` — class registry / dispatch
  (Phase 4)
- `~/.cache/ihc/sources/ghc-bignum-1.3/src/GHC/Num/` —
  reference for signatures and semantics
- Today's PRs `#131` / `#134` / `#136` — foundation already
  on master

## Verification

Per phase:

1. **Phase 1:** ad-hoc Coverage fixture that constructs
   `VPrimObj PrimBigNat` and round-trips through pattern matching.
2. **Phase 2.A–F:** per-tranche fixtures testing the new primops
   directly (see `prelude_decode_double_int64.hs` precedent from #131).
3. **Phase 3:** `case (IS 5#) of IS k -> ...` round-trip;
   `case (IP someBigNat) of IP bn -> ...` round-trip.
4. **Phase 4:** `case (5 :: Integer) + 3 of n -> n` returns 8.
5. **Phase 5:** the float→Int shims can be deleted; existing
   `floor` / `ceiling` Coverage fixtures stay green; new
   fixtures pin the source-loaded path.

End-to-end smoke: `print (floor (1.5 :: Double) :: Int)` prints
`1` after the float→Int graduation, going through source-loaded
`Integer` arithmetic the whole way.
