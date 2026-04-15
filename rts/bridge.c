/*
 * IHC — glue for the GHC RTS and dyld.
 *
 * Phase 0 has no symbols yet. The file exists so ihc.cabal's c-sources list
 * keeps compiling and so Phase 1 has a place to add:
 *   - dlopen/dlsym wrappers for foreign imports
 *   - bridges to GHC RTS allocation (allocate, newCAF, ...)
 *   - helpers the stencils will call
 */

#include <stddef.h>

// Placeholder symbol so the translation unit is non-empty and ar(1) keeps it.
int ihc_bridge_placeholder(void) { return 0; }
