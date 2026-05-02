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

The kernel listener is up reliably on `127.0.0.1:3099`, but
`socketAcceptB` is still not reached: warp's setup chain after `listen`
spins in host code without entering `acceptLoop`.
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
  E.mask_ acceptLoop`) hangs at 100% CPU before entering the accept
  loop. `curl --max-time 60` times out with zero response bytes.
* **The May-2 instrumentation pass corrected the prior diagnosis.**
  Counters in `eval`, `force`, `apply`/`applyIP`, `runIOVal`,
  `bindDispatch`/`seqDispatch`/`fmapDispatch`, `lookupInstanceMethod`,
  `classMethodDispatcher`'s value-directed dispatch, and the
  env-fallback hook all *flatline* once the program hangs (frozen at
  e.g. `forces=271 apply=293 eval=430 runIO=41 fallback=38 lookupI=4
  bind=7 seq=9 fmap=0 dispatch=6`). The CPU is therefore burning
  entirely in compiled host code, **not** the interpreter.
* `sample` shows the hot path is **`GHC.Internal.Data.Typeable.Internal`**:
  `Lc4jT_info` and `Lc4k8_info` (~25% combined) are local closures
  inside the `Typeable` machinery (immediate neighbours of
  `mkTrApp_info`, `sameTypeRep_info`, `showTypeable_info` in the
  binary), with `Data.ByteString.Internal.Type.compareBytes` /
  `eq_info` / `GHC.Classes.zeze_info` (`==`) accounting for another
  ~30%. `Lc4Anc_info` (~6%) is in `IHC.Scheduler` and references
  `compareBytes`, `Just`, and `Nothing` — i.e. a `Map ByteString`
  lookup helper. Last interpreter activity: `force #271` on the
  `runSettingsConnection` lambda, then `applyIP` consumed up to
  293 calls (lambda's set/getConn/app args + descent into the
  `runSettingsConnectionMaker` chain) — and then everything stops
  while the host keeps spinning.

The remaining work is **identifying the host-level `Typeable`-driven
hot loop** that the warp setup chain triggers between
`runSettingsConnection` and `acceptConnection`. Plausible suspects:

1. A repeated `try`/`catch` cycle inside warp's `withII` /
   `mkAutoUpdate` chain, each iteration computing `TypeRep`s for
   exception matching.
2. Some derived-`Typeable` dispatch that hits a non-terminating case
   in `containers`/`hashable` instance derivation when fed our
   interpreter-shaped values.

Either way the trail starts at the binary symbols `Lc4jT_info`
(0x101a60410-relative-to-text in a recent build) and `Lc4Anc_info`
(`IHC.Scheduler.o` offset 0x2dc0) — disassembly shows both call
into bytestring `compareBytes` and the `Maybe` constructors.

Earlier follow-ups that landed but proved insufficient:

1. **HashMap-ification of `ClassRegistry` and `MethodTable`** (commit
   in this branch). Both went from `Map`-backed to `HashMap`-backed,
   eliminating log-n `compareBytes` per dispatch lookup. The change
   is correct but the warp hang persists, because dispatch is not the
   actual hot loop (counters above prove it).
2. **Unified `runIOVal` between `IHC.Eval` and `IHC.Builtins`**: the
   two modules previously kept independent copies; bind/seq/fmap
   dispatch in `Builtins` was using its own runIOVal, hiding it from
   diagnostic counters added to the `Eval` copy. Now both paths
   share one definition.

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
