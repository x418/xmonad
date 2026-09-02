# Design

[river](https://github.com/riverwm/river) is a Wayland compositor that hands
window management policy to a separate process over
`river-window-management-v1`.  That is what lets xmonad run on Wayland
without compositing, and what lets `M-q` swap the window manager under live
clients.

## Decisions

**No libwayland.**  Bindings are generated from the protocol XML
(`util/generate-protocol.hs`) over `store-core`.  The one thing that needs C
is descriptor passing (`src/cbits/wl-fd.c`).

**The API is a strict subset of upstream xmonad's.**  If something cannot be
faithfully ported it is not exported, so a config fails at the unportable
call, at compile time.  `tests/api/check-subset.sh` enforces it; river-only
API lives in `XMonad.River`.

**Window management follows river's two sequences.**  Window management
state (dimensions, focus, bindings) may change only inside a manage sequence;
rendering state (position, stacking, borders, visibility) is applied at
`render_finish`.  So `windows` does not lay out: the layout runs once at the
end of the manage sequence, after every queued action, and produces a `Plan`.

**Two threads.**  The *loop* (the main thread) owns the connection and never
runs user code, so river is always answered.  The *worker* owns `XState` and
runs every `X` action -- bindings, hooks, layouts -- serialised, in the order
asked.  River holds input for the seat until a sequence is answered, so a
blocked action costs window management, never the session.

**Restart over a unix socket** (`XMonad.River.Control`): the request can be
refused with a reason, and its listener has a thread of its own so it can
interrupt a wedged loop.

## Architecture

### Ownership

One writer per field.  Where two threads meet, the mechanism says how.

```
riverWindows / riverOutputs / riverSeats   loop writes, worker reads   IORef
riverKeyBindings, rtPointerBind, rtArmed,
  rtGrabbed, rtDisarm, rtHovered, rtPending,
  rtSeqNo, rtSent, rtAsked, rtLast*         loop only                   IORef
riverCapture, riverExtraKeys, riverOverlays,
  riverOverlayPos, rtLayoutMoved            worker writes, loop reads   atomicWriteIORef / atomicModifyIORef'
riverPlacements, riverRestack, riverBorders,
  riverAfterLayout, riverDragOrigin,
  riverGeometry, riverSizeHints, rtAdopted  worker only                 IORef
riverDirty                                 worker sets, loop swaps     TVar Bool   (wakes the loop)
riverMailbox, rtJobs                       any thread posts            TVar [a]    (wakes the loop)
shPlan, shSeqDone                          worker publishes            TVar        (wakes the loop)
riverNowOps                                worker queues               TVar [Op]   (wakes the loop)
riverOps                                   worker queues, loop drains  atomic IORef, inside a sequence
```

The `Connection`'s request path (`request`, `newObject`, `setListener`) is
atomic, so the worker may create surfaces for decorations; reading,
dispatching and flushing are the loop's alone.  The one sequence-bound
request contrib made from the worker, `set_position` on a shell surface, is
recorded in `riverOverlayPos` and applied by the render sequence.

### The loop

One STM transaction waits on the socket (`threadWaitReadSTM`) and on
everything else that can wake it: posted actions, loop jobs, the dirty flag,
a now-op, a plan that landed late.  Each pass drains the mailboxes, sends
`manage_dirty` if anything asked, sends the now-ops, flushes, waits.

### A manage sequence

1. The loop numbers the sequence, drains the pending binding actions and
   hands `manageSequence n acts` to the worker.
2. The worker: restore a predecessor's state (first sequence only), drop what
   river closed, reconcile screens with outputs, adopt new windows through
   the manage hook, run the actions, lay out, publish the `Plan`, mark `n`
   done.
3. The loop waits up to `planGraceMicros` for *that* sequence number.  If it
   landed, closed objects are destroyed.  Either way the plan in hand is
   transmitted -- bindings for new seats, the one-shot ops, dimensions,
   focus -- and `manage_finish` is sent.
4. A plan that lands after its sequence was answered wakes the loop, which
   sends one `manage_dirty` for it.  Nothing is lost; it is one sequence late.

The render sequence transmits the plan's rendering half.  Both transmissions
send only what differs from the last one sent -- river keeps rendering state
between frames -- and check every reference against the objects river still
has, since the plan may be older than the maps.

### Plan and Op

Most of what a sequence sends is a restatement and is a value (`Plan`):
placements, borders, visibility, stacking, focus.  A few requests are
one-shot effects -- `close`, `pointer_warp`, `set_capabilities` -- and are
`Op`s, drained as they are sent.  Re-sending a plan is free; re-sending an op
is a bug.

### Input routing

Anything that must be coherent with a key press is loop state.  A submap or
a hold-to-cycle is an `InputCapture` the config writes and the loop arms
inside the sequence that carried the key press, atomically with it; the loop
reports which key fired by index and the config runs the action.  A capture
ends on its key, an unbound key, the watched modifier's release, or a
deadline, whichever claims the slot first.  `Ctrl-Alt-Shift-Escape` is bound
outside the config and cannot be disabled; it closes every prompt and
re-enables every binding.

### Prompts

A prompt is an ordinary Wayland client on a second connection and its own
thread (`XMonad.River.Client`): a layer surface with exclusive keyboard
interactivity, which is the only way to get real key events, dead keys and
compose.  Its teardown runs whatever happens, because a surface holding the
keyboard with nobody reading it is a session with no keyboard.

## Open questions

- **Submap teardown is a sequence late.**  `enable`/`disable` are legal only
  inside a sequence and a submap ends on a key press outside one, so the
  config's bindings come back at the next sequence.
- **A submap can arm late.**  If the worker is behind, the sequence goes out
  without the submap and the globals are live for a round trip.  A real
  guarantee needs submap prefixes declared to the loop ahead of time, which
  is a contrib change.
- **The state file is written by the worker.**  A wedged worker degrades a
  restart to the last committed state (`restartGraceMicros`).  The loop could
  write it from the last published plan, at the cost of `ReleaseResources`
  becoming best-effort.
- **A dead worker.**  `userCode` catches per action; a worker that died would
  leave a loop that answers river and does nothing.  It needs a supervisor.
- **Compositor-side fullscreen.**  `river_window_v1.fullscreen` is not used;
  fullscreen is the layout's, with `inform_fullscreen` telling the client.

## Assumptions about river

`river_window_v1.identifier` is stable across a restart and arrives before
the first `manage_start`.  `tests/headless-restart.sh` checks both.
