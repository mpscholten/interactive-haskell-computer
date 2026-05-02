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

This example **does not yet complete an HTTP request** but no longer
hits the VFun-leak bug. The blocker is now a downstream
`setFdOption`/`Fd` pattern dispatch issue.

What works:

* The whole warp / wai / http-types / network / streaming-commons /
  auto-update / time-manager dependency tree loads from source.
* `Network.Socket.socket`, `bind`, `listen` execute via host-backed
  builtins.
* The four original dry-run blockers (bare-import re-exports, record
  patterns in instance methods, `{-# LANGUAGE Strict #-}`,
  `UnboxedTuples`) are all resolved and have coverage fixtures under
  `test/Fixtures/Coverage/`.
* The VFun-leak bug (where `acceptConnection`'s body had `void`
  rewritten through `Network.Wai.Handler.Warp.Types.void` and
  evaluated to a `<function>` instead of an `IO ()`) is **fixed** —
  see the `directRewritePairs` foreign-alias-target detection in
  [src/IHC/Scheduler.hs:3573](src/IHC/Scheduler.hs:3573).

What does not work:

* The post-listen path now reaches `setSocketCloseOnExec → setFdOption fd CloseOnExec True`
  inside warp's `runSettingsSocket`, but a 3-arg function with
  patterns `[(Fd fd), opt, val]` is matched against arguments that
  don't have an `Fd` constructor wrapper. The error fires inside
  `errorB` with:
  `Non-exhaustive patterns in function: [[PCon "Fd" [PVar "fd"], PVar "opt", PVar "val"]]`.
  No source-level `setFdOption` exists in the cache (the unix
  package isn't shipped), so this points at our env-fallback or
  rewrite logic resolving the bare `setFdOption` to a synthesised
  source-style function instead of the host-backed
  [`setFdOptionB`](src/IHC/Builtins.hs:5692) builtin.

The next session should:

1. Re-add a print at the call site where `setFdOption` is being
   dispatched (instrument `apply` for `(EVar "setFdOption")` callers
   in Run.hs's body chain) to see which Val the lookup returns.
2. If it's a VFunIP (user-defined lambda), trace back through
   `discoverInModule` and the rewrite map to find which module
   contributed the source-style body.

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
