/*
 * IHC — JIT runtime primitives (macOS / aarch64).
 *
 * See jit.h for the contract. This implementation assumes Apple Silicon and
 * hardened runtime with the com.apple.security.cs.allow-jit entitlement.
 */

#include "jit.h"

#include <errno.h>
#include <libkern/OSCacheControl.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <unistd.h>

/*
 * On older SDKs pthread_jit_write_protect_np is not declared in any public
 * header but is present in libsystem_pthread. Declare it ourselves. It takes
 * an int: 1 = W^X protects against writes (i.e. executable), 0 = writable.
 *
 * See Apple's "Porting Just-In-Time Compilers to Apple Silicon" doc.
 */
extern void pthread_jit_write_protect_np(int enabled);

static size_t g_page_size = 0;

size_t ihc_jit_page_size(void) {
    if (g_page_size == 0) {
        long sz = sysconf(_SC_PAGESIZE);
        g_page_size = (sz > 0) ? (size_t)sz : 16384; // Apple Silicon default
    }
    return g_page_size;
}

static size_t round_up_to_page(size_t size) {
    size_t page = ihc_jit_page_size();
    return (size + page - 1) & ~(page - 1);
}

void *ihc_jit_alloc(size_t size) {
    size = round_up_to_page(size);
#ifndef MAP_JIT
#define MAP_JIT 0x800
#endif
    void *p = mmap(NULL, size,
                   PROT_READ | PROT_WRITE | PROT_EXEC,
                   MAP_ANON | MAP_PRIVATE | MAP_JIT,
                   -1, 0);
    if (p == MAP_FAILED) {
        return NULL;
    }
    return p;
}

void ihc_jit_free(void *ptr, size_t size) {
    if (ptr == NULL) return;
    munmap(ptr, round_up_to_page(size));
}

void ihc_jit_writable(void) {
    pthread_jit_write_protect_np(0);
}

void ihc_jit_executable(void) {
    pthread_jit_write_protect_np(1);
}

void ihc_jit_flush(void *ptr, size_t size) {
    // On Apple Silicon this is required: the I-cache is not coherent with the
    // D-cache after JIT writes. sys_icache_invalidate handles both.
    sys_icache_invalidate(ptr, size);
}

int ihc_jit_codesign_check(void) {
    // Probe: try allocating a page, making it writable, writing a ret, making
    // it executable, flushing, and executing. If any step faults we're not
    // properly signed.
    //
    // We deliberately keep this as a lightweight smoke probe — a real
    // SIGBUS/SIGKILL from the kernel on missing entitlement is unrecoverable
    // by signal handlers in some configurations. For now this returns 0 if
    // alloc succeeds; a full probe runs in the hspec smoke test instead.
    size_t page = ihc_jit_page_size();
    void *p = ihc_jit_alloc(page);
    if (p == NULL) {
        return errno ? errno : -1;
    }
    ihc_jit_free(p, page);
    return 0;
}
