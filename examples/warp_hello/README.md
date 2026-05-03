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

## Current state (2026-05-03)

The hang in warp's setup chain is **fixed**.  `curl
http://127.0.0.1:3099/` now connects, and the server starts responding
to the request — but the response itself fails partway through with an
`IhcException: IOError`, leaving curl with `Recv failure: Connection
reset by peer`.  The listener stays up for the brief window before
that exception unwinds the bracket and closes the socket.

What works:

* The whole warp / wai / http-types / network / streaming-commons /
  auto-update / time-manager / unix dependency tree loads from source.
* `Network.Socket.socket`, `bind`, `listen` execute via host-backed
  builtins; the listener appears within ~10 s.
* The four original dry-run blockers (bare-import re-exports, record
  patterns in instance methods, `{-# LANGUAGE Strict #-}`,
  `UnboxedTuples`) are all resolved.
* **VFun-leak fixed**: `directRewritePairs` no longer treats
  foreign-alias sentinels (bodies like `EVar "OtherMod.name"`
  inserted as memoization hints) as local definitions.
* **setFdOption pattern-dispatch fixed**: `setFdOption` is now in
  `ffiBuiltinNames` so the unix package's source-level
  `setFdOption (Fd fd) opt val` definition (which calls
  `c_fcntl_read`/`c_fcntl_write` we don't FFI-back) doesn't shadow
  our host implementation.
* **`findNameInImports` exponential blowup fixed (this commit)**: the
  named-reexport resolver in `IHC.Scheduler` was using a per-path
  `[ByteString]` visited list, which let every recursive entry into
  the same module re-explore its full import graph.  For warp's
  `Network.Wai.Handler.Warp.Imports → Control.Applicative`
  reexport chain, a single `<$>` lookup triggered ~19,000
  `findNameInImports` calls in a few seconds — Lc4jT_info /
  Lc4Anc_info hot symbols seen in the May-2 sample were this
  recursive Map.lookup chasing. Switched to a shared
  `IORef (Set ByteString)` so each module is explored at most once
  per query.  Resolving runSettingsConnectionMaker now completes
  immediately and warp proceeds into `withII` and `acceptConnection`.

What does not work:

* `IhcException: IOError` is thrown somewhere between the connection
  setup (warp's `socketConnection set s`) and the response write.  The
  trace right before the throw shows the `raiseIO# se` re-throw path
  — a `SomeException` is caught and re-thrown, indicating warp's
  exception machinery is firing on something missing (e.g. a host
  primop returning `IOError` from `EAGAIN` retry, or an unimplemented
  TLS / sendfile shim).  Listener is up briefly, curl handshakes
  succeed, but the response bytes never arrive.

The remaining work is to identify which IO action throws the
`IOError`.  Likely candidates:

1. `Network.Socket.ByteString.recv` / `Sock.sendMany` host wrapper
   throwing on EAGAIN without retrying via `threadWaitRead`.
2. `connSendFile` / `connSendAll` pulling from a host shim that
   isn't wired up for the buffer types warp constructs.
3. An `evaluate` deep inside `socketConnection` forcing a thunk that
   was built lazily and now references a missing FFI symbol.

Earlier follow-ups that also landed:

1. **HashMap-ification of `ClassRegistry` and `MethodTable`** —
   eliminates log-n `compareBytes` per dispatch lookup.
2. **Unified `runIOVal` between `IHC.Eval` and `IHC.Builtins`** —
   one definition with VIO, source-built `VCon "IO"`, and `VCon "STM"`
   cases.

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
