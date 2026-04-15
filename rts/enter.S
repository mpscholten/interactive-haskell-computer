// IHC — STG-style enter/apply hot loop (aarch64, Apple ABI).
//
// Phase 0: empty placeholder. rts_enter / rts_ap_p land in Phase 1.
// Kept as a real file so ihc.cabal's asm-sources entry compiles cleanly once
// we flip it on.

    .text
    .align 2
    .globl _ihc_enter_placeholder
_ihc_enter_placeholder:
    ret
