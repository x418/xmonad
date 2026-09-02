# Review and refactor

A review of the backend as taken from mgsloan (2026-09-02), and what was done
about it.  Findings first, then the checklist.  `DESIGN.md` describes the
result; this records the path.

## Findings

### Correctness (concurrency)

| # | Defect | Fixed |
|---|---|---|
| C1 | `rtPending` written non-atomically on the loop, drained atomically on the worker: actions dropped or run twice | loop-owned, handed to the worker as an argument |
| C2 | `riverWindows` had two writers (`rwNew` from the worker) | worker keeps its own adopted set |
| C3 | border overrides written by both threads | worker-only; forgotten in `reapClosed` |
| C4 | blind `writeIORef` on the worker against `atomicModifyIORef'` on the loop, no fence | `atomicWriteIORef` |
| C5 | the worker created bindings on the connection inside `manageSequence` | bindings created on the loop, in `transmitManage` |
| C6 | the capture deadline thread sent `manage_dirty` itself | posts a job to the loop |
| C7 | a plan published after the 8 ms wait was never transmitted | the loop waits on the plan serial and sends `manage_dirty` once |
| C8 | a drag whose seat vanished never ended; every later drag ignored | ended in `reapClosed` |
| C9 | SIGCHLD reaping disabled without a double fork: a zombie per `spawn` | open, see checklist |
| C10 | "landed" tested "serial moved", which a late previous sequence also does; `reapObjects` could destroy windows the `WindowSet` still held | exact sequence numbers |
| C11 | `riverDirty` and now-ops set by the worker outside a sequence did not wake the loop | TVars in the loop's wait |
| C12 | an exception in `manageSequence` left `inManageSeq` set forever; `rtBoundSeats` never pruned | `catchX`; pruned in `reapObjects` |
| C13 | contrib created and committed surfaces from the worker; `set_position` from there could land outside a sequence | atomic request path; overlay positions in the plan |

### Performance

| # | Issue | Done |
|---|---|---|
| P1 | two threads forked and killed per loop pass | `threadWaitReadSTM` in one `atomically` |
| P2 | `awaitPlan` polled in 2 ms steps | `registerDelay` |
| P3 | the 8 ms budget carried correctness | 50 ms, and correctness no longer depends on it (C7) |
| P4 | startup hook submitted every sequence | once |
| P5 | every render restated everything; the protocol keeps rendering state | diffed against the last transmission |
| P6 | state file parsed twice on restore | open (needs an API change to `readStateFile`) |
| P7 | `WindowAttributes` built for every window every sequence | lazy map |
| P8 | `Runtime` built with 25 positional `<*>` | record syntax |
| P9 | nine process globals | `RiverState` fields; `Display` carries it |

### Architecture

Preserved: `StackSet` byte-identical; `X`, `XState`, `XConf`, `XConfig`;
the hook set; `Event` as a `Message`; the exported-subset rule.

| # | Deviation | Done |
|---|---|---|
| A1 | `windows` does not lay out | correct and necessary; documented once |
| A2 | process-level globals | `RiverState` (`Display' X`) |
| A3 | `Runtime` aliased `RiverState`'s IORefs | `rtState` |
| A5 | `XMonad.River.Keyboard` dead | deleted |
| A6 | `planMustLand` never read; DESIGN.md described a design not built | deleted; DESIGN.md rewritten |
| A8 | `Operations.hs` haddocks one declaration off; duplicated `-- |` lines | reattached |
| A9 | `sigCHLD` handling commented out | open (C9) |
| C6 | one 1400-line `WM.hs` | split by thread and phase |

### Protocol

| # | Item | Done |
|---|---|---|
| R1 | `window_interaction` ignored: no click-to-focus | focus on interaction |
| R2 | fullscreen never granted or informed | `inform_fullscreen` via `XMonad.River.informFullscreen`; contrib's hooks call it |
| R3 | `set_capabilities` never sent | fullscreen only, on adoption |
| R4 | CSD move/resize requests ignored | start the interactive op; `inform_resize_*` around a resize |
| R5 | `wl_output` names ignored | bound by registry name; `XMonad.River.outputNames` |
| R7 | `inform_resize_start/end` unused | used |
| R8 | "river forgets rendering state" | wrong; see P5 |

## Checklist

- [x] A  thread split made true (C1-C8, C10-C13, P1-P4)
- [x] B1 diff-based transmit (P5), B3 lazy attributes (P7)
- [ ] B2 single parse of the state file (P6)
- [x] C1 globals into `RiverState`; `Display` as `Display' X`
- [x] C3 `Keyboard.hs` deleted; C4 `planMustLand` deleted; C6 `WM.hs` split
- [ ] C7 `xfork` double-forks with the grandchild's pid over a pipe (C9)
- [x] D1-D4 click-to-focus, inform_fullscreen + capabilities, CSD drags, output names
- [x] E1 `Operations.hs` haddocks; E3 `DESIGN.md`; E4 `README.river.md`
- [ ] E2 comment trim: `Client.hs`, `Connection.hs`, `Wire.hs`, `Core.hs`, `ManageHook.hs`
- [ ] regenerate `tests/api/river/*.golden` on a Linux build (the `Display` line was edited by hand)
- [ ] run `tests/check-all.sh` and the headless suites on skyali; add the C7 regression (a config whose action overruns the grace) to `tests/headless-river.sh`
