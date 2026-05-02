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

The kernel listener is up reliably on `127.0.0.1:3099`, but
`socketAcceptB` is still not reached: warp's setup chain after `listen`
spends all CPU in interpreter work without entering `acceptLoop`.
`curl http://127.0.0.1:3099/` connects (the listen backlog absorbs the
SYN) but receives zero response bytes.

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
  inserted as memoization hints) as local definitions. Previously
  `acceptConnection`'s `void $ E.mask_ acceptLoop` rewrote `void` to
  `Network.Wai.Handler.Warp.Types.void`, whose body forwarded to
  `GHC.Internal.Data.Functor.void` (a `VFunIP`, not a `VIO`) — the
  do-block `>>` saw a function as its first arg and the bracket
  exited with the function as its terminal result.
* **setFdOption pattern-dispatch fixed**: `setFdOption` is now in
  `ffiBuiltinNames` so the unix package's source-level
  `setFdOption (Fd fd) opt val` definition (which calls
  `c_fcntl_read`/`c_fcntl_write` we don't FFI-back) doesn't shadow
  our host implementation. `setFdOptionB` also unwraps the `Fd`
  newtype wrapper for source-constructed values.

What does not work:

* `socketAcceptB` is never reached. After bind/listen and
  `setSocketCloseOnExec` succeed, warp's setup chain
  (`runSettingsConnectionMakerSecure → withII → acceptConnection →
  E.mask_ acceptLoop`) spends all CPU in interpreted-thunk evaluation
  before entering the accept loop. `curl --max-time 60` times out
  with zero response bytes; the server keeps grinding.
* The hot path is still env-lookup / class-method dispatch
  (`Data.ByteString.compareBytes`, `eq_info`, `==` typeclass
  dispatch, `stg_ap_*` machinery) — same flavour of slowness flagged
  in the May 1 memoization commit (`ef9e6fb`).

The remaining work is **interpreter throughput on the post-listen
path**, not source loading or primops. Two follow-ups:

1. Profile-guided HashMap-ification of the still-Map-backed
   `ClassRegistry` (currently
   `Map (ByteString, [ByteString]) MethodTable`).
2. Closure compilation: lift the `eval`/`force`/`apply` inner loop
   so each thunk has a precompiled entry function instead of an
   AST-walk per evaluation.

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
