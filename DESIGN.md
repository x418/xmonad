# Design

Why this port is shaped the way it is. [`README.river.md`](README.river.md)
describes what works and how to run it; this describes the decisions behind it,
and the one architectural change still ahead.

## The premise

river's `main` implements no window management at all. Position, size, focus,
keybindings and decorations are all deferred to a separate process speaking
`river-window-management-v1`. That is what makes an xmonad port possible rather
than merely conceivable: a layout is pure code again, and `M-q` hot-swaps the
window manager without disturbing the compositor or its clients.

Two coincidences carry the config language across unchanged: xkbcommon reuses
X11's keysym numbering, and `river_seat_v1.modifiers` reuses X11's modifier
mask values. `mod4Mask` is still 64 and `xK_Return` is still `0xff0d`, so
`"M-S-<Return>"` means the same thing on both backends with no translation
layer.

## Decisions

**No libwayland.** The wire format is host-endian and 32-bit aligned, which is
exactly what `store-core`'s `Poke`/`Peek` do natively, so the codec is a few
hundred lines of Haskell (`XMonad.River.Wire`). The one thing that genuinely
needs C is `SCM_RIGHTS` fd passing, which `network` cannot express — about a
hundred lines in `src/cbits/wl-fd.c`. The payoff is that event dispatch is
ordinary Haskell rather than callbacks reached through `wl_proxy_marshal`'s
variadic interface.

**Protocol bindings are generated and checked in.** `util/generate-protocol.hs`
turns the XML into Haskell. Generating at build time was rejected: it would put
a custom `Setup.hs`, an XML parser and a network fetch in the path of every
rebuild, and the `M-q` recompile loop is what this project most needs to keep
fast. The XML itself is *not* vendored — it is downloaded from pinned upstream
revisions into a gitignored `protocol/`, so provenance is a one-line diff rather
than an opaque blob.

**The API is a strict subset of upstream's.** If something cannot be faithfully
ported, it is not exported: an absent name fails at compile time naming the file
and line, where an inert one fails at runtime in the last place anyone will
look. Everything river adds lives in `XMonad.River`, so a config importing
`XMonad` sees upstream's names minus the unportable ones and never more.
`tests/api/check-subset.sh` enforces this against a recorded copy of upstream's
interface.

**X11 spellings are kept where the concept survives.** `Rectangle`, `Window`,
`Event`, `SizeHints` live in `XMonad.River.Types` with river's semantics. This
is not imitation — it is what lets the layout arithmetic and the manage-hook
algebra be the same code on both backends instead of two copies that drift.
Nothing in `src/` is named `Graphics.X11`.

**State is split the way river splits it.** *Window management state* —
dimensions, focus, bindings — may only change between `manage_start` and
`manage_finish`. *Rendering state* — position, stacking, borders, hide/show —
may change in either sequence but only applies at `render_finish`. So layout
runs in the manage sequence because it must propose dimensions there, and its
results are stashed and applied during the render sequence that follows.

Rendering state is *restated in full* every frame: river keeps no memory of it,
which is why a per-window border override has to be stored and re-sent rather
than issued once.

**Bindings are deferred, not immediate.** A binding fires outside any sequence,
where window management state may not be touched. So the action is queued and
river is asked for a sequence with `manage_dirty`. This is the same deferral
river's own reference window manager uses.

**Workspaces are `hide`/`show`.** river has no workspace concept. A window on an
off-screen workspace is simply hidden.

**Prompts are a second connection.** The management protocol reports which
*binding* fired, never which key was pressed — right for a window manager,
useless for a text field. So a prompt opens its own connection and behaves as an
ordinary client on it: a `zwlr_layer_shell_v1` surface with keyboard
interactivity, real `wl_keyboard` events and the real keymap. Dead keys,
compose, input methods and key repeat all work, none of which a binding per
keysym could have provided.

**One thread per connection, enforced by interface.** `Connection` buffers
requests in `IORef`s and is not thread-safe. `XMonad.River.Client` forks a
thread that owns its connection outright and never hands it out; callers can
only redraw or close, both posted to a mailbox. Cross-thread wakeups go through
`XMonad.River.Mailbox`, a queue plus a self-pipe that sits in the loop's wait
set — X11 gave this away free via `sendEvent` to the root window, and Wayland
has no equivalent.

**Restart keeps the session.** `sendRestart` throws an async exception into the
loop, which does `stop` → `finished` → `exec`; river keeps every client alive
across the swap. Window placement survives because `StateFile` is keyed on
`river_window_v1.identifier`, which belongs to the window rather than to the
window manager's connection.

## The event loop, and the problem with it

Today one thread owns the connection *and* `XState`, and calls into user code:
binding actions run inside the manage sequence (`runPending`, `WM.hs:580`).

That is worse on river than the equivalent is on X11, and the difference is not
in the exported API — upstream's loop is equally single-threaded and blocks just
as completely. The difference is river's contract. Input events are queued
per-seat and drained only while `wm.state == .idle && !wm.scheduled.dirty`
(`river/Seat.zig:360`), and the drain resumes in exactly one place: after the WM
has answered *both* `manage_start` and `render_start`. So anything the window
manager does inside a sequence is added directly to the time during which nobody
in the session can type — not just window manager bindings, but all keyboard and
pointer input, to every client.

Pressing a bound key already stops the drain before we see it
(`XkbBinding.pressed` calls `dirtyWindowing`), and so does releasing it. The
practical consequence: a config action that shells out and waits freezes the
whole session for its duration. Ctrl+Alt+F-key still works — VT switching is
matched before the queue — and that is the only thing that does.

Deferring the action to just after `manage_finish` does not fix this: river is
still non-idle through the render sequence, and the key release re-dirties state
the moment the action resumes blocking. The loop has to stop running user code
altogether.

## Planned architecture: a worker thread and a published plan

Invert who owns what.

- A **worker thread** owns `XState` and runs *all* user code — binding actions,
  manage hooks, `runLayout`, window adoption — serialized, in upstream's order.
- The **loop** owns the connection and nothing else. It never runs user code.

Two immutable snapshots cross between them, each swapped atomically, so neither
side ever sees a torn read.

Loop → worker, the compositor's view:

```haskell
data World = World
  { worldWindows :: !(M.Map Window RiverWindow)
  , worldOutputs :: !(M.Map Output RiverOutput)
  , worldSeats   :: !(M.Map Seat   RiverSeat)
  }
```

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
  , planFocus      :: !(M.Map Seat FocusTarget)  -- FocusWindow w | ClearFocus
  , planBindings   :: !(M.Map ObjectId Bool)     -- submaps enable/disable these
  }

-- Delivered once, then dropped.
data Op
  = OpClose Window
  | OpWarpPointer Seat Position
  | OpPointerOpStart Seat PointerOp | OpPointerOpEnd Seat
  | OpSetTiled Window Word32
  | OpDecorations Window Decorations
```

Both live behind one `IORef (Plan, [Op])`: a single `atomicModifyIORef'`
replaces the plan and drains the ops to empty.

`manage_start` then means "transmit the current plan, `manage_finish`". If the
worker has published nothing new, the loop re-affirms the plan it already has —
a valid, cheap, consistent sequence. Response time is bounded by serializing a
value and writing a socket, with no user code in the path, so a blocked action
degrades to exactly upstream's failure mode: window management stops until it
returns, and the session keeps running.

Three things this forces:

- **Liveness filtering is the loop's job, at transmit time.** The worker's
  `World` is always slightly stale, so a plan can name a window river has since
  closed. The `M.member win known` guards that `applyLayout` and
  `renderSequence` do today do not disappear — they move, and they must cover
  every reference: placements, borders, visible, raised, focus, and each op. One
  missed reference is a protocol error, which disconnects the window manager.
- **Compositor events become worker work.** `window`/`output`/`seat` mutate
  `XState`, so the loop can only forward them. A window that appears while an
  action is blocked is not adopted until the action returns — which is precisely
  what X11 does.
- **The manage-hook ordering guarantee becomes load-bearing.** A window absent
  from the current plan must stay hidden until a plan includes it, or it flashes
  on screen unmanaged.

**Latency, and a bounded wait.** A plan costs every binding an extra round trip:
today a binding's effect lands in the sequence it triggered, afterwards it lands
one sequence later. The mitigation is for the loop to wait a bounded few
milliseconds for a publication before finishing the sequence, falling back to
re-affirming. Fast actions land immediately; slow ones cannot wedge anything.
This is not the timeout that was tried and rejected — that one bounded the
damage of a block that still happened, and its bound was on how long a person
thought. This one bounds a wait for a computation that either finished or did
not, and blocking is impossible either way.

**What it buys beyond responsiveness:** a wedged action becomes killable.
`throwTo` the worker and the loop never notices, which generalizes
`closeAllPrompts` from "only helps if what is stuck is a prompt" into a real
escape hatch.

**What does not change:** the exported API. Actions still run to completion,
serialized, in order; a blocking helper can keep blocking and keep returning its
`String`.

## Known issues and open questions

**Blocking user actions freeze the session.** The above is the plan; none of it
is built yet. Everything that shells out and waits is affected, and the only
current escape hatch is `closeAllPrompts`, which helps only if what is stuck is
a prompt.

**The shared-`IORef` audit is the real surface area of that change.** `XConf`
carries about eleven of them and each needs an owner. `rtBindings` and
`rtSubmap` are the awkward pair: they are deliberately shared between the IO
callbacks and `X` code today, which is exactly the sharing the split breaks.

**Submaps become a race.** "Is a submap open" moves to the worker, but the
`ate_unbound_key` callback runs on the loop. A key arriving as a submap opens or
closes is currently serialized by there being one thread, and will not be.

**Restart is the highest-risk path in that refactor.** `sendRestart` throws into
the loop, but the teardown it triggers — `broadcastMessage ReleaseResources >>
writeStateToFile` — is worker work. If the worker is wedged in the very action
being escaped, restart must abort it first, then run teardown, without losing
state. Getting this wrong drops the session instead of restarting it.

**Abort granularity would be "since the last publish".** `throwTo` mid-action
loses the `StateT` frame, so an action that made three `windows` calls and then
hung leaves the first two applied. Defensible — partial application looks the
same on X11 — but it is a decision, and it argues for publishing on every
`windows` rather than at the end of an action.

**A dead worker would be worse than a crash.** Today `userCode` catches per
action. A worker that dies under a live loop leaves a window manager that still
answers sequences but responds to nothing, which looks alive. It needs a
supervisor that restarts it from published state or exits so river releases the
session.

**Nothing tests the loop.** The suites cover `XMonad.StackSet` and the wire
codec. The concurrency invariants — no torn reads, ops delivered exactly once,
liveness filtering complete — are precisely what property tests cannot reach.
`tests/headless-river.sh` and `tests/headless-prompt.sh` are the only harnesses
that run against a real compositor, and they would need extending.

**Module bodies are unchecked for drift.** `check-subset.sh` pins the exported
API against upstream's, but nothing verifies that the modules described as
"identical" still are, or that "patched" ones have not quietly diverged further.

**Two protocol facts are river's rather than ours.** That
`river_window_v1.identifier` is stable across a restart, and that it arrives
before the first `manage_start`, are both properties of the compositor.
`tests/headless-restart.sh` checks them, because a river upgrade could change
either without anything here failing to compile.

**Should river mediate state handover?** Passing window placement to a successor
through a state file works because both processes share a filesystem. A channel
across `stop` → `finished` → `exec` would not depend on that. Not blocking
anything; still the tidier design.
