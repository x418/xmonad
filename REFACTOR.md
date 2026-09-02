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
| C9 | SIGCHLD reaping disabled without a double fork: a zombie per `spawn` | `xfork` double-forks; the grandchild's pid comes back over a pipe |
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
| A9 | `sigCHLD` handling commented out | SIGCHLD at its default; `xfork` reaps (C9) |
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
- [x] C7 `xfork` double-forks with the grandchild's pid over a pipe (C9)
- [x] D1-D4 click-to-focus, inform_fullscreen + capabilities, CSD drags, output names
- [x] E1 `Operations.hs` haddocks; E3 `DESIGN.md`; E4 `README.river.md`
- [ ] E2 comment trim: `Client.hs`, `Connection.hs`, `Wire.hs`, `Core.hs`, `ManageHook.hs`
- [ ] regenerate `tests/api/river/*.golden` on a Linux build (the `Display` line was edited by hand)
- [ ] run `tests/check-all.sh` and the headless suites on skyali; add the C7 regression (a config whose action overruns the grace) to `tests/headless-river.sh`

## Second review (2026-09-02)

The result above, read against river `main` (`100fd955`): the protocol XML,
`WindowManager.zig`'s `ensureWindowing`/`ensureRendering`, and where
`XkbBinding.zig` sends `pressed` (from `manageStart`, never at press time).
The design holds: exact sequence numbers, the stale-plan/fresh-map discipline,
rendering state legal in either sequence.  What did not:

### Defects

| # | Defect | Fixed |
|---|---|---|
| D1 | the panic chord re-enabled the globals and forgot `rtArmed` without destroying the capture's bindings: a submap's keys eaten from every client until restart | the panic drops the capture and sets `rtDisarm`; the sequence its press precedes tears down |
| D2 | `ate_unbound_key` cleared `riverCapture` with no generation check; closed a capture armed since | `claimCapture` by `rtArmedGen` |
| D3 | `grabKeysUpDown` swapped the action table while the old bindings were alive: wrong action for a key | the table and `OpGrabKeys` carry a generation |
| D4 | `modifiers_watch` sent to a version-2 seat object: protocol error on river < 0.4 | version checked in `whileModifiersHeld` and `armCapture` |
| D5 | `modifiers_watch` never withdrawn: a manage sequence per modifier change, forever, after the first hold-to-cycle | `modifiers_watch 0` on disarm |
| D6 | `OpGrabKeys`/`armCapture` addressed removed seats | `liveSeats` |
| D7 | `inManageSeq` stayed set if the worker was killed mid-sequence | `finally` |
| D8 | the worker killed for overrunning the restart grace was not replaced | forked again on the same queue |
| D9 | `Disconnected`/`ProtocolError` escaped every handler: no state file, `--restart` timed out | `onWayland` |
| D10 | a second `--restart` after a refused one found no handler | `supervise` |
| D11 | `restart` recorded `(cmd, [])` | `getArgs` |
| D12 | `willFloat` asked "is it floating" from a hook that runs before the float | asks what upstream's `manage` asked |
| D13 | the log hook ran in `windows` and again at the end of the sequence | once, `riverLogDue` |
| D14 | `nominateLayerOutput` matched the screen origin to the output's; a bar on the left or top edge moved it | containment |

### Performance

| # | Issue | Done |
|---|---|---|
| Q1 | `queueAction` sent `manage_dirty` for a press river had already scheduled a sequence for: an empty manage+render sequence per key | dirty only for what survives a pass; one per wait |
| Q2 | `propose_dimensions` re-sent every sequence while the client's size differed: a configure per sequence per terminal | re-proposed on change, not on difference |
| Q3 | every render frame rebuilt and diffed everything, including river's own frames | nothing sent for the same plan, windows and overlays |
| Q4 | two full maps over every window per sequence for attributes and hints | placed rectangles only; attributes built when asked |
| Q5 | `registerDelay` per sequence | checked first |
| Q6 | lazy `writeIORef` of the state, the placements and the transmission maps | forced |
| Q7 | `riverPlacements` an assoc list; `updatePlacement` O(n) per `op_delta` | open |
| Q8 | 4096-byte `recvmsg` and a copy per read | open |

### Protocol

| # | Item | Status |
|---|---|---|
| S1 | every request in the phase its state allows; `set_position` from the manage sequence is legal (`ensureRendering` accepts `.manage`; the per-request text said otherwise until 2026-09-01, which the pin bump takes) | ✅ |
| S2 | versions: manager ≥ 4 for `identifier`; river 0.4.6 advertises 5; xkb `get_seat` ≥ 2, `modifiers_watch` ≥ 3 | ✅ after D4 |
| S3 | `pointer_resize_requested` edges ignored: always bottom-right | open, low |
| S4 | a client resizing itself (a render sequence with no manage) leaves a float's recorded rectangle behind | open, low |
| S5 | `river_decoration_v1`, `sync_next_commit`, `set_clip_box`, `set_dimension_bounds` unused | DESIGN.md open questions |

### Checklist

- [x] D1–D14, Q1–Q6; protocol pins to river `main` `100fd955` and wayland 1.26.0, with a `check-all.sh` step
- [x] `tests/headless-submap.sh`: the panic chord, and an action slower than the plan grace (the C7 regression above)
- [ ] B2 single parse of the state file (P6); E2 comment trim
- [ ] Q7, Q8; S3, S4
- [ ] regenerate `tests/api/river/*.golden` on a Linux build (`RiverState` gained and lost fields)
- [ ] run `tests/check-all.sh` and the headless suites on skyali; in river's debug log, one `manage sequence finish` per key press, and none for a bare modifier after a hold-to-cycle has ended
