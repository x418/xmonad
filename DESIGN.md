# Design

[river](https://github.com/riverwm/river) is a Wayland compositor which
communicates with a separate window management process. This allows for a port of XMonad to Wayland without dealing with compositing. This architecture also allows for recompile hot-swapping.

## Decisions

**No libwayland.** Instead, protocol bindings are generated from XML. They use `store-core` which provides fast serialization. The one thing that needs C is fd passing.

**API is a strict subset of upstream xmonad.** The goal is to be able to run
xmonad configurations with minimal modification. If something cannot be
faithfully ported, it is not exported. River-only APIs are exported from
`XMonad.River`.

**Window changes are deferred.** A keybinding action can make changes to window
state. These changes do not take effect immediately - instead they are deferred.

**Two threads: protocol and worker thread.** River sometimes blocks event
handling while waiting for the window management process. If responses were
blocked on user code, this could cause a freeze of all event handling (keyboard
and pointer events in all windows). To address this, a protocol thread ensures
that responses are always sent promptly. A separate worker thread runs user
code, owns `XState`, and communicates with the protocol thread.

**`xmonad --restart` handled via a unix socket.** Before, X11 messages were used
for restart. Because it is a socket rather than a signal, the request can be
answered. Refusing to restart -- the successor's executable is gone, the worker
had to be aborted -- reaches the terminal that asked, with an exit status,
instead of only the session log.

The listener has a thread of its own rather than an fd in the event loop's wait
set. The request most worth delivering is the one sent to a window manager that
has stopped reading, so a listener sharing the loop's fate could not serve it;
on its own thread it can still interrupt a wedged loop, which is what keeps
`xmonad --restart` an escape hatch. The listening descriptor is close-on-exec,
without which the successor of a restart inherits it, concludes another window
manager owns the path, and quietly accepts connections nobody reads.

**Restart hands over with a bounded grace period.** A restart request arrives on
the protocol thread, which asks the worker to finish its current action, waits a
little, and only then tears down and writes state.

## Architecture: a worker thread and a published plan

Who owns what.

- A **worker thread** owns `XState` and runs *all* user code — binding actions,
  manage hooks, `runLayout`, window adoption — serialized, in upstream's order.
- The **loop** owns the connection and nothing else. It never runs user code.

Loop → worker, the compositor's view: the window, output and seat maps. Written
only by the loop, read by the worker, each behind its own `IORef` so a read is a
pointer load and never a torn one. The worker's view is therefore always
slightly stale, which is why transmitting filters against the live maps rather
than trusting the plan.

Worker → loop, what the compositor should be told. It splits in two, and the
split is the crux of the design: most of what a sequence sends is a *total
restatement* and is therefore a value, but a few requests are one-shot effects
that must be delivered exactly once. Re-sending a restatement is free;
re-sending a `close` kills a second window.

```haskell
-- Restated in full on every sequence.  Safe to re-transmit verbatim.
data Plan = Plan
  { planSerial     :: !Int                       -- monotonic; lets the worker
                                                 -- see when a plan has landed
  , planPlacements :: ![(Window, Rectangle)]     -- bottom-to-top, floats last
  , planBorders    :: !(M.Map Window (Dimension, RGBA))
  , planVisible    :: !(S.Set Window)
  , planRaised     :: ![Window]
  , planFocus      :: !FocusTarget               -- FocusWindow w | ClearFocus
  , planLayerDefault :: !(Maybe (ObjectId, Maybe ObjectId))
  , planMustLand   :: !Bool                      -- see below
  }

-- Delivered once, then dropped.
data Op
  = OpClose Window
  | OpWarpPointer Seat Position
  | OpPointerOpStart Seat PointerOp | OpPointerOpEnd Seat
  | OpSetPosition Window Position Position
  | OpProposeDimensions Window Dimension Dimension
  | OpUseDecorations Window Bool
  | OpCaptureInput [(KeyMask, KeySym)] KeyMask Bool Int
  | OpGrabKeys [(KeyMask, KeySym)] | OpUngrabKeys
```

A second, smaller queue carries what needs no sequence and must not wait for
one — `exit_session`, `stop`, `set_xcursor_theme`. Those are drained on every
pass of the loop, because waiting for a sequence would mean waiting for
something nothing is going to ask for.

The plan is replaced; the ops are drained. Re-sending a plan is free, and
re-sending an op is a bug.

**Ops come before the thread split, not after.** `Connection` buffers requests
in `IORef`s and is not thread-safe, so a worker thread may not touch it at all
— and until `kill`, `warpPointer` and the interactive drags stop issuing their
own requests, user code touches it constantly. Routing them through the queue
is what makes the worker possible, so it has to land first.

Nothing reachable from `X` issues a request any more. Each case was split by
which thread the work belongs to: the window, output and seat handlers do their
protocol work and their bookkeeping on the loop and hand back only the hook a
config might have installed; `reapClosed` keeps the `WindowSet` half while
`reapObjects` destroys the objects; the layer-surface default became a field of
the plan; `requestManageSequence` sets a flag the loop drains rather than
sending `manage_dirty` itself.

**The worker publishes at every `windows`.** Upstream's `windows` applies
immediately — it issues the X requests and the screen updates — so an action
that lays out, works, and lays out again shows the first result straight away.
Matching that means one publish per `windows` call rather than one when the
action returns. Aborting an action therefore leaves exactly what was already on
screen: nothing un-happens. This is not atomic, and an action needing two
`windows` calls for one logical change can be interrupted between them — but an
exception in that same action does the same under X11.

`manage_start` then means "transmit the current plan, `manage_finish`". If the
worker has published nothing new, the loop re-affirms the plan it already has —
a valid, cheap, consistent sequence. Response time is bounded by serializing a
value and writing a socket, with no user code in the path, so a blocked action
degrades to exactly upstream's failure mode: window management stops until it
returns, and the session keeps running.

Four things this forces:

- **Liveness filtering is the loop's job, at transmit time.** The worker's
  `World` is always slightly stale, so a plan can name a window river has since
  closed. The `M.member win known` guards that `applyLayout` and
  `renderSequence` do today do not disappear — they move, and they must cover
  every reference: placements, borders, visible, raised, focus, and each op. One
  missed reference is a protocol error, which disconnects the window manager.
- **Compositor events split.** The bookkeeping is the loop's; only the hook a
  config installed goes to the worker. A window that appears while an action is
  blocked is recorded at once but not adopted into the `WindowSet` until the
  action returns — which is precisely what X11 does.
- **The manage-hook ordering guarantee becomes load-bearing.** A window absent
  from the current plan must stay hidden until a plan includes it, or it flashes
  on screen unmanaged.
- **Input routing is loop state.** Anything that has to be coherent with a key
  press — which bindings are enabled, whether a submap is open — belongs to the
  loop rather than the worker. `submapNextKey` becomes an op naming the key set;
  the loop arms the bindings, asks for the next unbound key, holds the deadline,
  and posts back which key was chosen, leaving the worker only the action to
  run. `grabKeys` and `whileModifiersHeld` have the identical shape and get the
  identical treatment. Without this the `ate_unbound_key` callback would be
  reading a slot the worker is concurrently writing.

**Latency, and a bounded wait.** A plan costs every binding an extra round trip:
today a binding's effect lands in the sequence it triggered, afterwards it lands
one sequence later. The mitigation is for the loop to wait a bounded few
milliseconds for a publication before finishing the sequence, falling back to
re-affirming. Fast actions land immediately; slow ones cannot wedge anything.
This is not the timeout that was tried and rejected — that one bounded the
damage of a block that still happened, and its bound was on how long a person
thought. This one bounds a wait for a computation that either finished or did
not, and blocking is impossible either way.

**A plan may require its sequence.** Some publications are only correct if they
land in the sequence that provoked them. Arming a submap is the case: it has to
be atomic with the key press that opened it, or the config's own bindings are
still live when the next key arrives. Such a plan is marked, and the loop waits
longer for it than the ordinary few milliseconds. The wait is still bounded —
an unbounded one is the freeze this design exists to remove — so it is a strong
preference and not a guarantee.

**What it buys beyond responsiveness:** a wedged action becomes killable.
`throwTo` the worker and the loop never notices, which generalizes
`closeAllPrompts` from "only helps if what is stuck is a prompt" into a real
escape hatch.

**What does not change:** the exported API. Actions still run to completion,
serialized, in order; a blocking helper can keep blocking and keep returning its
`String`.

## Known issues and open questions

FIXME: address these

**Submap teardown cannot happen when the key fires.** `enable` and `disable`
are manage-sequence-only and a submap key fires outside one, so restoring the
config's bindings is always deferred to the next sequence — today through the
mailbox, and in the loop-owned version through a pending-teardown slot drained
at the next transmit. Worth stating because the arming side is the opposite: it
has to be atomic with the press, so the two halves of a submap live in
different sequences by construction.

**Nothing tests submaps.** `headless-prompt.sh` covers prompts, not this, and
the failure mode is a session in which every binding is disabled. A test that
arms a submap, presses a key and asserts the globals come back belongs
alongside the change rather than after it.

**A submap can still arm late.** Loop-owned input routing removes the data race,
but not the delay: if the worker is behind an action that overran its wait, the
sequence goes out without the submap and the config's globals are live for a
round trip — up to about 100ms when a client is slow to ack a configure, which
is inside the speed at which people type a chord. Pressing the second key there
runs its global binding instead. A real guarantee needs the loop to know the key
set *before* the worker runs, which means declaring submap prefixes rather than
discovering them inside an opaque `X ()`; that is a change on the contrib side,
recorded here so it is not rediscovered.

**Where the state file gets written from is unsettled.** Publishing at every
`windows` means the loop always holds a snapshot it could serialize itself, so a
restart need not wait on the worker at all: write from the last publish and
exec. That would take the worker off the restart path entirely, at the cost of
`broadcastMessage ReleaseResources`, which is user code and would become
best-effort — skipped when the worker is wedged. Cheaper here than under X11,
where there were server resources to hand back, but still a behaviour change.
The alternative is to keep writing from the worker and accept that a wedged
worker degrades the restart.

**How long the grace period should be is unsettled**, both the ordinary one and
the longer wait for a plan that requires its sequence. Too short and a submap
arms late; too long and every restart feels slow.

**Cooperative cancellation is not an option, and this is why.** Setting a flag
for the worker to check cannot work for the case that matters: a blocked
`readProcess` never returns to check anything. So aborting is `throwTo` or
nothing. It is still worth throwing before an exec rather than just exec'ing,
so that `bracket` and `finally` in user code get to run — with a bound on the
unwinding, since a `finally` that blocks must not hold a restart hostage.

**A dead worker would be worse than a crash.** Today `userCode` catches per
action. A worker that dies under a live loop leaves a window manager that still
answers sequences but responds to nothing, which looks alive. It needs a
supervisor that restarts it from published state or exits so river releases the
session.

## Assumptions

**Two protocol facts are river's rather than ours.** That
`river_window_v1.identifier` is stable across a restart, and that it arrives
before the first `manage_start`, are both properties of the compositor.
`tests/headless-restart.sh` checks them, because a river upgrade could change
either without anything here failing to compile.

## Upstream River issues

**Should river mediate state handover?** Passing window placement to a successor
through a state file works because both processes share a filesystem. A channel
across `stop` → `finished` → `exec` would not depend on that. Not blocking
anything; still the tidier design.
