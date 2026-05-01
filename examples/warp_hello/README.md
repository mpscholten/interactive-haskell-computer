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

## Current state (2026-05-01)

This example **does not yet complete an HTTP request**. It is checked in
to anchor the warp-server objective and provide a reproducer for the
remaining gap.

What works:

* The whole warp / wai / http-types / network / streaming-commons /
  auto-update / time-manager dependency tree loads from source.
* `Network.Socket.socket`, `bind`, `listen` execute via host-backed
  builtins; `lsof -nP -iTCP:3099 -sTCP:LISTEN` shows the kernel
  listener within a few seconds.
* The four original dry-run blockers (bare-import re-exports, record
  patterns in instance methods, `{-# LANGUAGE Strict #-}`,
  `UnboxedTuples`) are all resolved and have coverage fixtures under
  `test/Fixtures/Coverage/`.

What does not work:

* `socketAcceptB` is never reached. After `listen` returns, the
  interpreter spends all CPU in interpreted-thunk evaluation before it
  ever enters Warp's `acceptLoop`. `curl http://127.0.0.1:3099/` connects
  at the kernel level (the listen backlog absorbs the SYN) but receives
  zero response bytes and times out.
* Sampling the running process with `sample <pid>` shows the main thread
  hot in `Data.ByteString.compareBytes` underneath
  `Data.List.filter`/`elem` and the IHC `schedule` /
  interpreter info tables. This is the same env-fallback chain
  flagged as "remaining spin is downstream" in the
  `Scheduler: memoise resolveImport + discoverInModule miss results`
  commit (ef9e6fb).

In short: the gap is no longer in source loading, parsing, primops, or
socket setup. It is the cost of demanding the lazy thunks that sit
between `listen` and `acceptLoop` in Warp's connection-manager and
timer-manager wiring. Closing it requires turning the env-fallback
hot path into something better than linear scans over ByteString lists.

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
