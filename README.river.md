# The river backend

```
stack build
```

This package builds against river/Wayland and nothing else. It began as a flag
over two source trees — upstream's `src/` frozen beside a `src-river/` — and
that is gone. What the second tree existed to prove is kept without it: this
fork's API must remain a strict subset of upstream's, and `tests/api/upstream/`
is upstream's own interface, recorded from a real checkout with
`tests/api/dump-api.sh tests/api/upstream ../xmonad` and committed, so
`tests/api/check-subset.sh` compares two files and builds nothing.

**Status: it manages a window against a real compositor.**

`tests/headless-river.sh` starts river on a headless backend — no display, no
seat, no GPU — runs this window manager against it, and asserts on river's
debug log. All four assertions pass: the connection is made and the wire codec
round-trips against a real server (55 globals enumerated correctly),
`river_window_manager_v1` binds at version 5, a client creates a toplevel, and
**river configures that window from the layout** — `sent 1 tracked
configure(s)`, which is the only signal that proves a layout reached the
compositor.

`tests/headless-prompt.sh` covers the other half of the input story, and is
the one to run when touching `XMonad.River.Client`. It stands a prompt's layer
surface up inside the same headless river — via `tests/river-prompt-spec.hs`,
which drives `startClient` directly — and asserts that a surface asking for an
exclusive keyboard grab never outlives the thread servicing it: the startup
watchdog closes one that can never read the keyboard, leaves alone one that
never wanted it, survives a draw callback that throws, and yields to
`closeAllClients`. River's own log is the check that matters — every
`'xmonad-prompt' mapped` must be matched by a `destroyed`.

That is one window, laid out once, on a headless backend, and prompts that open
and close but are never typed into. It is not a working desktop. What has *not*
run: drawing, real keyboard input, the mailbox, workspace switching, and
everything in xmonad-contrib. Those remain compile-time evidence only.

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

## State across a restart

**Fixed, and it needed no upstream change.** `M-q` keeps windows on the
workspaces you put them on, and `PersistentExtension` persists.

This was the one item here listed as a question for river's author. The
obstacle was real: river object ids are per-connection and recycled after
`wl_display.delete_id`, so an id written by one window manager means nothing to
its successor. The way out is `river_window_v1.identifier`, and reading the
compositor source answered both things that were unclear:

- **Is the identifier stable across a restart?** Yes, by design. It comes from
  the window's `ext_foreign_toplevel_handle_v1`, which belongs to the window
  and not to the window manager's connection. `Window.zig` creates one only if
  the window does not already have it, with a comment saying that case *is* the
  window manager restarting.
- **Does it arrive before the first `manage_start`?** Yes, by construction.
  `WindowManager.manageStart()` iterates every window — sending `window` and
  `identifier` for each — and only then sends `manage_start`.

So `StateFile` is keyed on identifier, and `restoreState` resolves them during
the first manage sequence, before screens are reconciled or anything is laid
out. `tests/headless-restart.sh` checks it against a real compositor, because
both facts above are properties of river rather than of this code, and a
compositor upgrade could change either without anything here failing to
compile. It restarts twice: one restart cannot distinguish a restore from a
restore that also re-manages everything it restored.

The second question — whether river should offer a channel for a window
manager to hand opaque state to its successor across `stop` → `finished` → exec
— is no longer blocking anything, but is still the tidier design. The
filesystem works because both processes share one; a compositor-mediated
handover would not depend on that.

`xmonad --restart` needed a rendezvous of its own, for the same reason. Under
X11 the second process put a client message on the root window and the server
delivered it; river mediates the window manager handover but offers no channel
between two window manager processes. The running one writes its pid to
`xmonad-river.pid` in the data directory, and `--restart` sends it `SIGUSR1`.
