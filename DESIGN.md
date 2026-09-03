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
rtBindings, rtPointerBind, rtArmed,
  rtArmedGen, rtGrabbed, rtDisarm,
  rtModWatcher, rtModWatched, rtHovered,
  rtPending, rtDirtySent, rtSeqNo, rtSent,
  rtAsked, rtWindowsGen, rtLast*            loop only                   IORef
riverCapture                               worker installs; the loop   atomicWriteIORef / atomicModifyIORef'
                                           claims it, by generation
riverExtraKeys                             worker writes (generation,  atomicWriteIORef; the loop fires a
                                           table), loop reads          binding only for its own generation
riverOverlays, riverOverlayPos             contrib writes, loop reads  atomicModifyIORef' (XMonad.Util.XUtils)
riverRestack                               worker only                 IORef
rtLayoutMoved                              worker writes, loop reads   atomicWriteIORef / atomicModifyIORef'
riverPlacements, riverGeometry,
  riverBorders, riverAfterLayout,
  riverDragOrigin, riverLogDue, riverUnsized,
  riverSubmapGen, inManageSeq, rtAdopted,
  rtRestored                               worker only                 IORef
riverRestart                               worker (restart) or loop    atomicWriteIORef; read by the loop on
                                           (--restart) writes          finished, long after either
stateRef (the XState)                      worker only, except the     plain IORef; the loop reads it only
                                           restart path                after killing the worker
riverDirty                                 worker sets, loop swaps     TVar Bool   (wakes the loop)
riverMailbox, rtJobs                       any thread posts            TVar [a]    (wakes the loop)
shPlan, shSeqDone                          worker publishes            TVar        (wakes the loop)
riverNowOps                                worker queues               TVar [Op]   (wakes the loop)
riverOps                                   worker queues, loop drains  atomic IORef; drained by the next
                                           sequence, so an emitOp outside one
                                           needs a manageDirty after it
connection outgoing requests               any thread queues, loop    TQueue (Encoded, [Fd]); bytes and
                                           drains                     descriptors are one atomic item
client registry                            client threads update and   atomicModifyIORef'; shutdown swaps
                                           shutdown drains             the registry before killing clients
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

A binding's press does not ask for a sequence.  River sends every input
event -- a press, a click, `pointer_enter`, `op_delta` -- from its own
`manageStart`, in the batch that precedes `manage_start`, so the action it
queues is drained by that `manage_start` in the same pass; a `manage_dirty`
as well would buy an empty sequence after every key.  Only what is still
queued when a pass ends -- posted from another thread, or a batch split
across two reads -- is asked for, and one request covers any number of them
until the `manage_start` arrives (`rtDirtySent`).

### A manage sequence

1. The loop numbers the sequence, drains the pending binding actions and
   hands `manageSequence n acts` to the worker.
2. The worker: restore a predecessor's state (first sequence only), drop what
   river closed, reconcile screens with outputs, adopt new windows through
   the manage hook, settle the floats river has sized since, run the
   actions, lay out, publish the `Plan`, mark `n` done.
3. The loop waits up to `planGraceMicros` for *that* sequence number.  If it
   landed, closed objects are destroyed.  Either way the plan in hand is
   transmitted -- bindings for new seats, the one-shot ops, dimensions,
   focus -- and `manage_finish` is sent.  Focus goes only to a window that
   has mapped (reported dimensions); a newly adopted window gets it one
   sequence later, which its first `dimensions` event asks for.  river would
   otherwise send `wl_keyboard.enter` before the client's first buffer,
   which JBR drops.
4. A plan that lands after its sequence was answered wakes the loop, which
   sends one `manage_dirty` for it.  Nothing is lost; it is one sequence late.

The render sequence transmits the plan's rendering half.  Both transmissions
send only what differs from the last one sent -- river keeps rendering state
between frames -- and check every reference against the objects river still
has, since the plan may be older than the maps.  A render sequence given the
same plan, windows and overlays as the last one -- river starts one whenever
a client changes its own size -- sends nothing at all.  Dimensions are
re-proposed when the ask changes or when a tiled client moves away from it,
not for as long as it merely differs: a terminal rounding to its cell would
otherwise be configured every sequence.  A float is never put back; it owns
its size.  A float adopted before its first `dimensions` -- every dialog --
has no size to propose and is proposed 0x0, river's "the window decides";
the sequence its first `dimensions` asks for centres it at what it decided.
Proposing the fallback instead (its minimum, or half the screen) was taken
literally by JBR, which shrank its dialogs to it.

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
deadline, whichever claims the slot first, and only for the generation that
armed it.  Teardown -- the capture's bindings destroyed, the config's
re-enabled, the modifier watch withdrawn -- happens in the next manage
sequence, the only place `enable` is legal.  `Ctrl-Alt-Shift-Escape` is
bound outside the config and cannot be disabled; it closes every prompt and
drops the capture, and the sequence its press precedes does the teardown.

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
- **A dead worker.**  The worker catches every synchronous exception per
  action, and the restart path replaces the one it kills; nothing watches
  for it dying any other way.
- **Compositor-side fullscreen.**  `river_window_v1.fullscreen` is not used;
  fullscreen is the layout's, with `inform_fullscreen` telling the client.
- **Decorations are shell surfaces.**  Contrib draws tab bars and overlays
  on `river_shell_surface_v1` and the render sequence positions and
  `place_top`s them every frame.  `river_decoration_v1`
  (`get_decoration_above` + `set_offset`) would have river move, hide and
  order them with their window, and `sync_next_commit` would make a
  decoration's frame atomic with the layout's; `set_clip_box` would let
  Magnifier keep off its neighbours' borders.  A contrib-scale change.
- **`riverPlacements` is an assoc list**, searched linearly by
  `floatLocation`, `windowUnderPointer` and every `op_delta`.

## Assumptions about river

`river_window_v1.identifier` is stable across a restart and arrives before
the first `manage_start`.  `tests/headless-restart.sh` checks both.

Input events are sent only from river's `manageStart`, so a binding's press
always arrives in the batch before a `manage_start` and with river already
in the manage state (`XkbBinding.zig` schedules, `Seat.manageStart` sends).
Two things rest on it: a queued action needs no `manage_dirty`, and the
panic chord's teardown lands in the sequence its press precedes.
