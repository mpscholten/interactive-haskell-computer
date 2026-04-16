# Warp Source-Load Diagnostic Findings

**Date:** 2026-04-16
**IHC version:** master HEAD f19b71f (Phase 2 — tree-walking evaluator)
**Warp version:** 3.4.12 — source at `~/.cache/ihc/sources/warp-3.4.12/`
**WAI version:** 3.2.4
**http-types version:** 0.12.4

---

## Probe Matrix

| Probe | Program | Result |
|-------|---------|--------|
| 1 | `import Network.HTTP.Types (status200); main = putStrLn "ok"` | **PASS** |
| 1b | `print (statusCode status200)` (re-exported field accessor) | **FAIL** – `unbound variable statusCode` |
| 2 | `import Network.Wai (responseLBS); main = putStrLn "ok"` | **PASS** |
| 2b | `let resp = responseLBS status200 [] "hello"` (construct Response) | **PASS** |
| 3 | `import Network.Wai.Handler.Warp (run); main = putStrLn "ok"` | **PASS** |
| 3b | `main = run 8080 $ \_ respond -> respond $ responseLBS status200 [] "Hi"` | **FAIL** – `unbound variable run` |
| 4 | `import Network.HTTP.Types.Status (statusCode)` (direct module import) | **FAIL** – `unbound variable a` in instance body |
| 5 | Local record field access `fooBar x` | **PASS** |
| 5b | Record pattern in function-equation LHS | **FAIL** – non-exhaustive patterns |
| 5c | Record pattern in case expression | **PASS** |
| 5d | Record pattern in instance method body | **FAIL** – `unbound variable a` |
| 6 | Record update `defaultSettings { settingsPort = 8080 }` | **SILENT FAIL** — exits 0 before print |
| 7 | `{-# LANGUAGE StrictData #-}` | **PASS** |
| 7b | `{-# LANGUAGE Strict #-}` | **SILENT FAIL** — exits 0 before first `let` binding |
| 8 | `import GHC.Conc.Sync (labelThread, myThreadId)` | **FAIL** – parse error `TkLUnbox` (UnboxedTuples) |

---

## Blocker #1 (Critical): Re-export Resolution — Bare Un-qualified Imports Not Followed

**Severity:** Blocks `run`, `statusCode`, and all gateway-module re-exports.
**Errors:** `unbound variable run`, `unbound variable statusCode`

`Network.Wai.Handler.Warp` exports `run` via a bare `import Network.Wai.Handler.Warp.Run` (no explicit import list). The scheduler's `followModuleReexports` only chases `ExportModule` entries. A bare un-qualified import in a gateway module is not treated as a re-export source.

Same issue as Aeson Blocker #1. The fix in `Scheduler.hs` — when a name fails to resolve, walk the module's bare (unqualified) imports and search them recursively — benefits every layered package.

---

## Blocker #2 (Critical): Record Patterns in Instance Method Definitions

**Severity:** Blocks `Eq Status`, `Ord Status`, and every class instance using record patterns.
**Error:** `unbound variable a` evaluating `Status { statusCode = a } == Status { statusCode = b } = a == b`

`Network.HTTP.Types.Status` defines:
```haskell
instance Eq Status where
    Status { statusCode = a } == Status { statusCode = b } = a == b
```

Record pattern variables are not bound in the environment when the instance method body runs. Case-expression record patterns work (probe 5c passes); function-equation record patterns in instance blocks do not.

**Fix needed:** `Parser.hs` / `Eval.hs` — instance method equations with record patterns on the LHS need the same binding treatment as case-alt patterns. Also affects function equations outside instances (probe 5b fails too).

---

## Blocker #3 (Critical): `{-# LANGUAGE Strict #-}` Causes Silent Exit

**Severity:** Blocks ALL warp modules. Warp's cabal declares `default-extensions: Strict StrictData` for GHC >= 8.
**Symptom:** Program prints first `putStrLn`, then silently exits 0 before any `let` binding executes.

The `Strict` extension makes all binding patterns strict by default. IHC's evaluator does not implement `Strict`-mode semantics. It appears IHC is silently short-circuiting the IO action chain at the first let-binding when `Strict` is active — possibly a mis-parsed `let !x = expr` that drops the continuation.

This affects every warp module: `Run.hs`, `Types.hs`, `Settings.hs`, `Buffer.hs`, etc.

**Fix needed:** Diagnose in `Eval.hs` why a `let` under `Strict` silently exits. The simplest fix may be to ignore `Strict` entirely (IHC is a lazy evaluator; the semantics difference is harmless for correctness) and just skip bang-pattern evaluation.

---

## Blocker #4 (Critical): `GHC.Conc.Sync` — UnboxedTuples Parse Failure

**Severity:** Blocks `labelThread` + `myThreadId` (used in `Warp.Run`), `threadWaitRead`/`threadWaitWrite` (used in `Network.Socket`).
**Error:** `parse error at GHC/Conc/Sync.hs:2:28 — TkLUnbox`

`GHC.Conc.Sync` uses `{-# LANGUAGE UnboxedTuples #-}`. IHC cannot parse `(# a, b #)` syntax. Since warp and network import from `GHC.Conc`, the scheduler attempts to load the source and hits this parse error.

**Fix needed:** Add `GHC.Conc.Sync` (and `GHC.Conc`) to `isBuiltinBackedModule` — this is justified (RTS scheduler primitives, no pure-Haskell alternative). Add `labelThread`, `threadWaitRead`, `threadWaitWrite`, `closeFdWith` to the builtin env.

**Related:** `Network.Socket` package requires 64 `foreign import` declarations across 14 files (some `.hsc`) — this is the single largest new work item for Phase 2.14. `Network.Socket` wraps POSIX syscalls; it must be host-backed.

---

## Novel Features Surfaced

### 1. `Strict` Extension (Warp Default)
All warp modules use `Strict StrictData`. Must be diagnosed before any warp module executes.

### 2. `Network.Socket` — Full FFI Infrastructure
64 `foreign import` declarations: `socket(2)`, `bind(2)`, `listen(2)`, `accept(2)`, `connect(2)`, `close(2)`, `ntohs`/`htons`/`ntohl`/`htonl`, `getaddrinfo`, `sendmsg`/`recvmsg`. Several `.hsc` files need `hsc2hs` preprocessing before they can be parsed. Must be host-backed under the minimum-builtin rule.

### 3. `GHC.Event` / `TimerManager`
`time-manager` (warp's connection-timeout library) uses `GHC.Event.registerTimeout` → `getTimerManager`. `GHC.Event` has `MagicHash`, `UnboxedTuples`, and FFI calls into the RTS event loop (`setIOManagerWakeupFd`, `getOrSetSystemTimerThreadEventManagerStore`). Must be host-backed.

### 4. Record Update Syntax — Silent Exit
Record update `rec { field = val }` causes silent exit 0 when the record was defined in an imported module. Separate from the record-pattern-in-instance bug. Needs diagnosis in `Eval.hs`.

### 5. `streaming-commons` — C cbits for UTF-8 Decode
`Data.Streaming.Text` uses `foreign import ccall "_hs_streaming_commons_decode_utf8_state"` from `cbits/text-helper.c`. Affects the warp response-body builder path. Either needs cbits compilation or a pure-Haskell fallback.

### 6. `hsc2hs` Preprocessing for `network`
Several `network` modules are `.hsc` files. IHC's module resolver expects `.hs` files. A preprocessing or stub-generation step is needed.

---

## Phase Assessment

**Warp is NOT Phase 2.14 in its original scope.** The scope is roughly **2–3× Phase 2.13** (bytestring tests).

**Breakdown:**
- Blockers #1 + #2 + record-update fix + Strict fix: ~4–6 days (these benefit aeson too)
- `GHC.Conc.Sync` + `GHC.Event` as host-backed: ~2 days
- `Network.Socket` host-backed shim (64 FFI bindings + hsc preprocessing): ~5–7 days
- End-to-end hello-world (actually bind and serve): ~2 days

**Recommended phasing:**
- **Phase 2.14a:** Fix Blockers #1–#3 + record update — gets warp to "parses, resolves, and evaluates up to `run`"
- **Phase 2.14b:** Add `GHC.Conc.Sync`, `GHC.Event`, `Network.Socket` as host-backed modules
- **Phase 2.14c:** Actually serve one HTTP request

---

## Dependency Table

| Package | FFI / Special? | Phase Readiness |
|---------|----------------|-----------------|
| `http-types` | Pure Haskell | Blocked by #1, #2 |
| `wai` | Pure Haskell | `responseLBS` works; deeper paths blocked |
| `warp` | `pread` + Strict | Import-only passes; run unresolved |
| `network` | 64 FFI decls, `.hsc` | Must be host-backed (Phase 2.14b) |
| `streaming-commons` | C cbits (text) | `Data.Streaming.Network` parseable; `Text` blocked |
| `auto-update` | `GHC.Event` | Must be host-backed |
| `time-manager` | `GHC.Event.registerTimeout` | Must be host-backed |
| `GHC.Conc.Sync` | UnboxedTuples, RTS | Must be host-backed (#4) |
| `GHC.Event` | UnboxedTuples, RTS FFI | Must be host-backed |
| `vault` | Pure Haskell | Not probed |
| `http-date` | Pure Haskell | Not probed |
| `hashable` | MagicHash | Likely needs primops (previously seen) |

---

## Files Referenced

- `~/.cache/ihc/sources/warp-3.4.12/Network/Wai/Handler/Warp.hs` — gateway module (bare re-imports)
- `~/.cache/ihc/sources/warp-3.4.12/Network/Wai/Handler/Warp/Run.hs` — `run` definition
- `~/.cache/ihc/sources/warp-3.4.12/warp.cabal` — `default-extensions: Strict StrictData`
- `~/.cache/ihc/sources/http-types-0.12.4/Network/HTTP/Types/Status.hs` — record patterns in Eq/Ord instances
- `~/.cache/ihc/sources/network-3.2.8.0/Network/Socket/Syscall.hs` — 64 FFI syscall wrappers
- `~/.cache/ihc/sources/network-3.2.8.0/Network/Socket/Types.hsc` — hsc2hs file, `closeFdWith`
- `~/.cache/ihc/sources/auto-update-0.2.6/Control/AutoUpdate.hs` — `GHC.Event` usage
- `~/.cache/ihc/sources/time-manager-0.3.2/System/TimeManager.hs` — `GHC.Event.registerTimeout`
- `~/.cache/ihc/sources/base-4.19.0.0/GHC/Conc/Sync.hs` — UnboxedTuples parse failure
- `/Users/marc/digitallyinduced/interactive-haskell-computer/src/IHC/Scheduler.hs` — `isBuiltinBackedModule`, `followModuleReexports`
- `/Users/marc/digitallyinduced/interactive-haskell-computer/src/IHC/Eval.hs` — instance method record patterns, record update
