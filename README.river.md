# The river backend

```
stack build                      # X11, byte-for-byte upstream behaviour
stack build --flag xmonad:river  # river/Wayland
```

**Status: it manages a window against a real compositor.**

`tests/headless-river.sh` starts river on a headless backend — no display, no
seat, no GPU — runs this window manager against it, and asserts on river's
debug log. All four assertions pass: the connection is made and the wire codec
round-trips against a real server (55 globals enumerated correctly),
`river_window_manager_v1` binds at version 5, a client creates a toplevel, and
**river configures that window from the layout** — `sent 1 tracked
configure(s)`, which is the only signal that proves a layout reached the
compositor.

That is one window, laid out once, on a headless backend. It is not a working
desktop. What has *not* run: drawing, prompts, keyboard input, the mailbox,
workspace switching, and everything in xmonad-contrib. Those remain
compile-time evidence only.

## Why river, and why this is possible

river's master branch does not implement window management at all. It defers
*all* policy — position, size, focus, keybindings, decorations — to a separate
process implementing [`river-window-management-v1`][wm-protocol]. Three
consequences matter:

1. **Custom layouts survive.** The window manager computes every window's
   geometry and hands it over via `river_node_v1.set_position` and
   `river_window_v1.propose_dimensions`. A layout is ordinary pure code again.
   This is the thing sway cannot offer at any price.
2. **`M-q` survives.** river hot-swaps window managers without restarting the
   compositor or its clients, so the recompile-and-restart loop is recoverable.
3. **The keymap ports as data.** xkbcommon reuses X11's keysym numbering and
   `river_seat_v1.modifiers` reuses X11's modifier mask values. `mod4Mask` is
   still 64, `xK_Return` still `0xff0d`, so `"M-S-<Return>"` means the same
   thing without translation.

[wm-protocol]: https://isaacfreund.com/docs/wayland/river-window-management-v1/

There is no dependency on `libwayland`. None of the river window management
protocols pass file descriptors, and fd passing (`SCM_RIGHTS`) is the only part
of the wire format that genuinely needs C — so the package carries about a
hundred lines of it in `cbits/wl-fd.c`, reached through `XMonad.River.Socket`,
rather than the whole of libwayland. That was forced by drawing: a `wl_buffer`
comes from a `wl_shm_pool`, which comes from `wl_shm.create_pool`, which takes
a descriptor. Exactly one request in the drawing path does. Serialization uses `store-core`,
whose `Poke`/`Peek` are host-endian and 32-bit aligned — exactly Wayland's wire
format. `binary` and `cereal` are big-endian and would route every field
through an escape hatch.

## Layout of the tree

`src/` is the X11 build and is **frozen**. `tests/check-x11-source.sh` asserts
it is byte-identical to the commit recorded in `tests/upstream-base.sha`, so the
river work cannot perturb the X11 build by accident. Changing it stays
possible — bump the SHA in the same commit, and that diff is the review signal.

`src-river/` is a complete second copy. It does not shadow `src/` by
`hs-source-dirs` precedence and shares nothing with it; the flag-off build never
looks at it. The alternative was to share the backend-independent modules and
let the rest shadow, which makes a module's provenance depend on a flag and on
directory order. Two honest copies read better than one file that means
different things on different days.

Nothing in `src-river/` is named `Graphics.X11`.

The cost of a second copy is drift, so `tests/check-copies.sh` sorts every
module into one of three categories and pins the membership exactly:

| | | |
| --- | --- | --- |
| **identical** | `XMonad.StackSet` | byte-for-byte. Pure, total, mentions neither backend — any difference at all is a bug |
| **patched** | `XMonad`, `XMonad.Core`, `XMonad.Layout` | a tracked divergence kept as a unified diff under `patches/`; applying it to `src/` must reproduce `src-river/` exactly |
| **rewritten** | `XMonad.Config`, `.ManageHook`, `.Main`, `.Operations` | different programs that agree on an interface; nothing to diff |

The patched category is where the rebase safety actually lives. `XMonad.Core`
is 84% shared with upstream — it holds `recompile`, `getDirectories` and the
whole compile path, none of which has anything to do with X11 — so when
upstream fixes something there, `patch` either applies the fix or conflicts
loudly. Either beats discovering months later that only one copy was fixed.
`tests/check-copies.sh --regen` rewrites the patches after a deliberate change.

A file moving between categories fails until the manifest says so, including a
file that becomes *more* similar: that usually means a river module has been
simplified back toward upstream, and whether it should still be a separate copy
is worth answering deliberately.

## The rule

**If something cannot be faithfully ported, it is not exported.**

A name that is present but inert is worse than one that is absent. Absent fails
at the call site, at compile time, naming the file and line, and whoever reads
that error learns something true. A no-op that typechecks teaches them nothing
until the behaviour is missing at runtime, by which point the backend is the
last place they will look.

The consequence is that river's API is a strict subset in both directions. 26
names the X11 build exports are gone, each justified by name in
[`tests/api/unportable.txt`](tests/api/unportable.txt); the 1458 names `XMonad`
re-exports from `Graphics.X11` are not re-exported at all; and river's eight
modules add **nothing**. Everything river offers that xmonad does not lives in
`XMonad.River`, so a config importing `XMonad` sees exactly the names the X11
build offers minus the unportable ones, and never more. Importing
`XMonad.River` is a config saying explicitly that it is a river config.

`tests/api/check-subset.sh` enforces all three: a dropped name without an entry
fails, a stale entry for a name river has since grown fails, and a
river-specific name appearing in `XMonad.Core` fails with the fix named — move
it to `XMonad.River`.

This is a real cost, and it falls on xmonad-contrib: a contrib module that
touches `withDisplay`, size hints, or window properties will not compile against
this backend. That is the intended outcome, not a regression to be papered over.

## What is implemented

| Area | Notes |
| --- | --- |
| Wire codec | `store-core`. 14 tests, including byte sequences derived from the spec. |
| Connection | Socket, object id recycling, registry, dispatch, roundtrip. |
| Protocol bindings | Generated from the XML in `protocol/`, checked in. |
| Manage/render loop | Layout and focus in the manage sequence; position, order, borders and hide/show in render — matching river's split of the two state categories. |
| Workspaces | river has no workspace concept; hidden workspaces use `river_window_v1.hide`/`show`. |
| Layouts | `LayoutClass`, `Tall`, `Full`, `Mirror`, `\|\|\|`. |
| Manage hooks | Run during the manage sequence, *before* the window is rendered — the ordering guarantee xmonad has and sway's IPC cannot give. |
| `title`, `className`, `appName` | From `river_window_v1.title` and `app_id`. Note river has no separate instance name, so `className` and `appName` are the same string. |
| Layer shell | Bound via `river_layer_shell_v1`, without which river closes every layer surface on sight. This is what makes prompts, notifications, wallpaper and bars appear at all, and its exclusive zones shrink the tiling area. |
| Screens | Reconciled from river outputs every manage sequence, ordered by position so screen ids are stable across reconnects. |
| Pointer warping | `river_seat_v1.pointer_warp`. |
| `M-q` | `sendRestart` throws an async exception into the event loop thread; the loop does `stop` → `finished` → `exec`. river keeps every client alive across the swap. |

## Runtime dependencies

Unlike the X11 build, some things are done by asking another program rather
than by asking a server. None is a build dependency, so none can be checked for
at compile time; each warns once on stderr if it is missing, rather than
silently doing nothing.

| needed for | program | why not done directly |
| --- | --- | --- |
| `XMonad.Util.XSelection` — the selection and clipboard | `wl-paste`, from `wl-clipboard` | Wayland offers the selection only to the client holding keyboard focus, and a window manager has no focused surface — it is not in the focus chain at all. The primary selection is a separate protocol again (`zwp_primary_selection_v1`). `wl-paste` works because it is a real client that can take focus for the instant it needs. |

Prompts are *not* on this list either, and the reason is worth knowing. A
prompt opens a **second Wayland connection** and behaves as an ordinary client
on it — a `zwlr_layer_shell_v1` surface with exclusive keyboard interactivity —
because the window management protocol deliberately does not deliver keys.
`river_seat_v1` reports which *binding* fired, never which key was pressed,
which is right for a window manager and useless for a text field. Going through
the client protocols instead gets real `wl_keyboard` events and the keymap, so
dead keys, compose sequences, input methods and key repeat all work; a binding
per keysym could not have done any of them.

Each connection is owned by exactly one thread. `Connection` buffers requests
in `IORef`s and is not thread-safe, so `XMonad.River.Client` forks a thread that
owns its connection outright and never hands it out: a caller can only redraw or
close, both posted to a mailbox. That is enforced by the module's interface
rather than by a comment, because GAPS.md §3 exists precisely because the rule
was broken once.

Drawing is *not* on this list. Prompts and decorations are rendered in-process
with cairo and pango, into a `wl_shm` buffer the compositor reads directly —
see `XMonad.River.Surface` and, on the contrib side,
`XMonad.Util.River.Draw`. Those are library dependencies of xmonad-contrib
rather than of xmonad, exactly as `X11-xft` is under X11, so a config with no
prompts and no decorations links neither.

## Known gaps

[`GAPS.md`](GAPS.md) covers what the working prototype this was rewritten from
learned by actually running: the missing reference window manager, the headless
test recipe, the hazard of blocking the event loop, that CI never builds this
backend at all, and the session integration `INSTALL.md` says nothing about.

- **Nothing has run against a live river.** See the status line at the top.
- **Interactive move and resize are wired but untested.** They run over river's
  seat operation cycle (`op_start_pointer` / `op_delta` / `op_release`) rather
  than by grabbing the pointer. `mouseDrag` turns river's *total offset since
  the drag began* back into the absolute position its callers expect, by adding
  the pointer position recorded at `op_start`. That assumes `pointer_position`
  has arrived by then, which is the part no unit test can check.
- **Multi-key submaps.** Needs a transient binding set installed for the prefix,
  using `ensure_next_key_eaten` so an unbound key cancels cleanly.
- **Floating geometry** is tracked in the `StackSet` but not yet applied during
  the render sequence.
- **`logHook` output has no consumer.** A Wayland bar wants `ext-workspace-v1`
  or a direct IPC.
- **State does not survive a restart** — see below.

## Worth fixing upstream in river

**Window manager state cannot survive a restart, because object ids cannot.**

xmonad serialises its `WindowSet` to a state file and reads it back after
`M-q`, so windows stay on the workspaces you put them on. That is not possible
here. River object ids are per-connection and are recycled after
`wl_display.delete_id`, so an id written by one window manager means nothing to
its successor. Writing the file is easy; reading it back correctly is the part
with no answer today. This is why `StateFile`, `writeStateToFile`,
`readStateFile` and `stateFileName` are absent, and why `PersistentExtension`
behaves exactly like `StateExtension`.

There may be a purely local fix, and it should be tried before asking anyone
upstream for anything: `river_window_v1.identifier` is documented as a unique
string that outlives the object, so resume state could be keyed on identifier
rather than object id and re-mapped as windows are re-advertised at startup.

What is genuinely unclear — and what the upstream question would be — is
whether that can be made reliable:

- Is `identifier` guaranteed to be delivered for every existing window before
  the first `manage_start`? If not, the first manage sequence has to lay out
  windows it cannot yet identify, and the restore either races or has to be
  deferred by a sequence.
- Is there, or should there be, a channel for a window manager to hand opaque
  state to its successor across the `stop` → `finished` → exec handover? river
  already mediates that handover, and it is the only participant that spans
  both processes.

I have not verified either against a running river or read the compositor
source closely enough to be confident, so this is a question to raise rather
than a bug to report.
