# GitHub Actions Workflows

## ci.yml — Build & Test

Triggers on every push and pull-request targeting `master` or `main`.

### Runner

`macos-14` (Apple Silicon / M1). This is **not negotiable**: IHC emits native
`aarch64` machine code and uses `MAP_JIT` pages, which require Apple Silicon
semantics. An `x86_64` runner cannot execute the JIT output.

### Steps

1. **Install Nix** via `cachix/install-nix-action@v27` with flakes enabled.
2. **Nix store cache** via `DeterminateSystems/magic-nix-cache-action@v8` —
   avoids re-downloading GHC (700 MB+) on every run by caching the `/nix/store`
   to GitHub's cache backend.
3. **Build** — `nix develop --command cabal build all`
4. **Sign test binary** — `make sign-test` applies the
   `com.apple.security.cs.allow-jit` ad-hoc entitlement; without this the test
   process is killed by the macOS kernel before any JIT page can be mapped.
5. **Test** — `cabal test ihc-test --test-show-details=streaming`; stdout/stderr
   are tee'd to `/tmp/ihc-test.log`.
6. **Upload test log** (on failure only) — uploads `/tmp/ihc-test.log` as the
   `test-log` artifact (retained 7 days).

### Pinned action versions

| Action | Version |
|---|---|
| `actions/checkout` | `v4` |
| `cachix/install-nix-action` | `v27` |
| `DeterminateSystems/magic-nix-cache-action` | `v8` |
| `actions/upload-artifact` | `v4` |

### Expected runtimes

| Cache state | Estimated wall-clock time |
|---|---|
| Cold (no Nix store cached) | ~12–18 min (GHC download dominates) |
| Warm (Nix store hit) | ~3–5 min |

### Cache invalidation

The Nix cache key is derived from the Nix store path hashes computed at
evaluation time, which in turn depend on `flake.lock`. If you need to force a
full cold rebuild, bump a `flake.nix` input and run `nix flake update` to
produce a new `flake.lock` — the next CI run will miss the cache and repopulate
it.
