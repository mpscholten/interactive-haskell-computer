/*
 * IHC — JIT runtime primitives (macOS / aarch64)
 *
 * Allocates MAP_JIT pages and toggles their write-protection per the Apple
 * Silicon hardened-runtime rules. The JIT page is writable on the *current
 * thread only* when ihc_jit_writable() is active, and executable otherwise.
 *
 * Lifecycle:
 *   1. ihc_jit_alloc(n)            -> void* page, read-execute
 *   2. ihc_jit_writable()          -- this thread: W ok, X not
 *   3. memcpy(..) into the page
 *   4. ihc_jit_executable()        -- this thread: X ok, W not
 *   5. ihc_jit_flush(ptr, size)    -- invalidate I-cache for the modified range
 *   6. ((fn)ptr)()
 */

#ifndef IHC_JIT_H
#define IHC_JIT_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Allocate an anonymous MAP_JIT region at least `size` bytes (rounded up to
 * the system page size). Returns NULL on failure (check errno). */
void *ihc_jit_alloc(size_t size);

/* Release a region previously returned by ihc_jit_alloc. */
void ihc_jit_free(void *ptr, size_t size);

/* Toggle this thread's JIT write protection. Must be bracketed per emit. */
void ihc_jit_writable(void);
void ihc_jit_executable(void);

/* Flush I-cache and D-cache for the given range. Must be called after the
 * last write and before the first execution of newly emitted code. */
void ihc_jit_flush(void *ptr, size_t size);

/* Returns the system page size (cached). */
size_t ihc_jit_page_size(void);

/* Sanity check: verifies the binary has the com.apple.security.cs.allow-jit
 * entitlement. Returns 0 on success, non-zero with a useful errno on failure.
 * Callers should abort with a friendly message if this returns non-zero. */
int ihc_jit_codesign_check(void);

#ifdef __cplusplus
}
#endif

#endif /* IHC_JIT_H */
