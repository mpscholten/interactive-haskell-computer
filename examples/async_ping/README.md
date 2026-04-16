# async_ping

Two threads ping-pong via a pair of `MVar ()`s. The main thread plays
"ping" 5 times; a forked thread plays "pong" 5 times. A third `MVar`
signals completion so `main` waits cleanly.

Demonstrates: `forkIO`, `MVar`, `newEmptyMVar`, `takeMVar`, `putMVar`,
multi-threaded IO, synchronization without STM.

## Run

```
nix develop -c cabal run exe:ihc -- run examples/async_ping/Main.hs
```

## Verify

```bash
nix develop -c cabal run exe:ihc -- run examples/async_ping/Main.hs | diff - examples/async_ping/async_ping.out
```
