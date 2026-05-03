# warp_hello

Minimal `Network.Wai.Handler.Warp` server that responds `Hello, Warp!` to
every request.

```haskell
import Network.HTTP.Types (status200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3099 $ \_ respond ->
    respond $ responseLBS status200 [] "Hello, Warp!"
```

## Run

```
nix develop -c cabal run exe:ihc -- run examples/warp_hello/Main.hs
```

## Current state (2026-05-03 — late)

The hang is **fixed**, and several follow-on bugs have been peeled
back.  `curl http://127.0.0.1:3099/` now reaches the listener and
TCP-handshakes successfully, but the server closes the connection
before sending a response — curl reports `Recv failure: Connection
reset by peer`.  The IHC process exits cleanly (return code 0) with
no exception printed: warp's `runSettings` returns instead of
spinning in the accept loop.

What works:

* The whole warp / wai / http-types / network / streaming-commons /
  auto-update / time-manager / unix dependency tree loads from source.
* `Network.Socket.socket`, `bind`, `listen`, `setSocketOption` and the
  unix `setFdOption` execute via host-backed builtins; listener
  appears within ~1 s.
* `mkAutoUpdate` from `auto-update` runs (one `forkIO` for the
  date-cache worker), `mask_` fires once for that fork.
* End-to-end TCP handshake works: curl connects to the listener.
* **Findings landed since the May-2 hang report:**
  * **`findNameInImports` exponential blowup fixed**: the
    named-reexport resolver in `IHC.Scheduler` was using a per-path
    `[ByteString]` visited list, which let every recursive entry
    into the same module re-explore its full import graph.  For
    `Network.Wai.Handler.Warp.Imports → Control.Applicative`, a
    single `<$>` lookup triggered ~19,000 calls in seconds.
    Switched to a shared `IORef (Set ByteString)`; resolving
    `runSettingsConnectionMaker` completes instantly.
  * **TVar primops route through `requireTVarPrim`**: `readTVar`,
    `writeTVar`, `modifyTVar'`, `readTVarIO` now unwrap the
    source-level `newtype TVar a = TVar (TVar# RealWorld a)` and
    further `newtype Counter = Counter (TVar Int)`-style wrappers.
    Previously each builtin only matched the bare
    `VPrimObj (PrimTVar tv)` and threw "not a TVar" on the first
    `Counter` shape warp constructs.
  * **`getAddrInfo` respects hints**: the builtin used to ignore the
    `hints` argument and pass `NULL` to the OS, which returned both
    UDP and TCP entries.  warp's `bindPortGenEx` then tried the UDP
    address first, with `setSocketOption sock TCP_NODELAY 1` failing
    EINVAL.  warp caught it via `tryAddrs` and retried with the TCP
    addr, but the per-attempt churn is wasted.  Now we build a host
    `struct addrinfo` from the `Maybe AddrInfo` Val (4 OS-relevant
    fields: flags, family, socktype, protocol).
  * **HashMap-ification of `ClassRegistry` and `MethodTable`** —
    eliminates log-n `compareBytes` per dispatch lookup.
  * **Unified `runIOVal` between `IHC.Eval` and `IHC.Builtins`** —
    one definition with VIO, source-built `VCon "IO"`, and
    `VCon "STM"` cases.
  * **VFun-leak fix** — `directRewritePairs` no longer treats
    foreign-alias sentinels as local definitions.
  * **`setFdOption` host route** — `setFdOption` is in
    `ffiBuiltinNames` so the unix package's source-level
    pattern-matched definition can't shadow our host impl.

What does not work:

* `runSettings` returns instead of running the accept loop.  Tracing
  shows: `mask_B` fires exactly once (mkAutoUpdate's `mask_ $ forkIO`
  in the date cache worker setup); `voidB` never fires; `socketAcceptB`
  never fires; `atomicallyB` never fires.  So the chain stops between
  `mkAutoUpdate` returning to `withDateCache` and `acceptConnection`
  being invoked.  Default settings have
  `settingsFdCacheDuration = 0` and `settingsFileInfoCacheDuration = 0`,
  so `withFdCache 0 action = action getFdNothing` and
  `withFileInfoCache 0 action = action getInfoNaive` should be
  near-trivial pass-throughs.  The next investigation step is to
  confirm whether `withFdCache`/`withFileInfoCache` actually invoke
  their continuation `action` — specifically whether the `case 0`
  pattern match is hitting in our interpreter for the `Int` value
  computed via `settingsFdCacheDuration set * 1000000`.

Reproduction:

```bash
direnv exec . cabal build exe:ihc
direnv exec . ./dist-newstyle/build/aarch64-osx/ghc-9.10.3/ihc-0.1.0.0/x/ihc/build/ihc/ihc \
    run examples/warp_hello/Main.hs &
IHC_PID=$!

# Listener appears for ~1 s then disappears as runSettings returns.
until lsof -nP -iTCP:3099 -sTCP:LISTEN | grep -q LISTEN; do sleep 0.05; done
curl -v --max-time 1 http://127.0.0.1:3099/   # connects, then "Recv failure"

kill "$IHC_PID"
```

The HashMap-backed Env (commit 993fef5) and the `try`-catches-all
fix (commit da212e0) closed the previous "silent exit before listen"
class of failures.

## Reproduce the diagnosis

```bash
nix develop -c cabal build exe:ihc
./dist-newstyle/build/aarch64-osx/ghc-9.10.3/ihc-0.1.0.0/x/ihc/build/ihc/ihc \
    run examples/warp_hello/Main.hs &
IHC_PID=$!
# Listener appears in 5–10 s
until lsof -nP -iTCP:3099 -sTCP:LISTEN | grep -q LISTEN; do sleep 1; done

# Sampling shows the env-fallback hot path
sample "$IHC_PID" 3 -file /tmp/ihc-warp-hello.sample
head -60 /tmp/ihc-warp-hello.sample

# curl times out: connect succeeds at kernel level, no bytes back
curl -v --max-time 5 http://127.0.0.1:3099/

kill "$IHC_PID"
```

## Related

* [`warp-dryrun-findings.md`](../../warp-dryrun-findings.md) — original
  blocker list (2026-04-16).
* [`.hermes/plans/2026-04-27_085219-warp-hello-readiness.md`](../../.hermes/plans/2026-04-27_085219-warp-hello-readiness.md)
  — first time the listener-up-but-no-bytes state was documented.
* [`.hermes/plans/2026-04-28_220353-warp-startup-debug-plan.md`](../../.hermes/plans/2026-04-28_220353-warp-startup-debug-plan.md)
  — pre-main hang plan; the hang itself is now resolved (commit c921fb4)
  but the post-listen spin remains.
