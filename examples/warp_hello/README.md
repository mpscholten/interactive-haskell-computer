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

## Current state (2026-05-02)

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

* `socketAcceptB` is never reached. The bracket's inner action returns
  a `<function>` (VFun) instead of an `IO ()`, so the bracket cleans up
  and `runIO` exits with the unrun function as the terminal value.
* Diagnostic instrumentation (added then reverted in this session)
  showed the leak originates inside three runIOVal call sites in
  `IHC.Builtins.hs`:
  * Line ~2471 in `seqDispatch`'s non-IO fallback — `>>` is invoked
    with a function-typed first arg.
  * Line ~2416 in `bindDispatch`'s non-IO fallback — `>>=`'s
    continuation result is a function.
  * Line ~5987 in `atomicallyHashB` — STM state-transformer `apply`
    returns a function instead of the `(# State#, a #)` tuple.
* The `HasCallStack`-traced runs reproduce reliably (warp setup
  evaluates, listener appears, then bracket teardown fires after a
  few seconds with `runIO non-VIO after 1 VIO unwraps: <VFun>`).

The next session should re-add HasCallStack-based instrumentation to
`runIOVal` and trace which source binding evaluates to a partial
application instead of an IO action — the bug is upstream of the three
detected sites and likely in either `Counter <$> newTVarIO 0` (from
warp's `newCounter`), the timer-manager `withII` chain, or one of
warp's eta-reduced point-free helpers (`runSettingsConnectionMaker`
takes 2 args but is invoked with 3).

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
