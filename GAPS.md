# Gaps carried over from the prototype

This repo is a rewrite of a working prototype (`~/env/xmonad-river` +
`~/env/xmonad-river-contrib`). The rewrite is better engineered — frozen X11
tree, tracked `patches/`, API goldens, no inert exports — but it was written
from the prototype's *design*, not its *operational experience*. The prototype
has since run against a live river, headless and on real hardware.

This records what that turned up.

Two scope notes. The keybinding vocabulary gap — no config being able to name
`mod4Mask` or any keysym — is **closed**: `XMonad.Core` exports the nine
modifier masks, the five button numbers and 347 keysyms, and
`XMonad.River.Keysym` exports `stringToKeysym`/`keysymToString`, with the
lookup tables reachable through `XMonad.River`. The contrib layer is no longer
absent either — `../xmonad-contrib-river` builds upstream contrib against this
backend, and 103 of its 328 modules compile; see its `SURVEY.md`.

## 1. No reference window manager

`~/env/xmonad-river/app/tinyrwm/Main.hs` — 419 lines, a port of river's own
reference WM written directly against `Connection` + the generated bindings,
with zero xmonad layer. This repo's only executable is `Main.hs`
(`main = xmonad def`).

Value is differential testing: a known-good client on the same protocol stack
separates "my window manager is wrong" from "my codec is wrong". It is what
established that

- a suspected watchdog timeout was a benign startup artifact, logged once
  before the WM connects; and
- `sent 0 tracked configure(s)` is river's normal per-cycle log line, not a
  failure to propose dimensions.

Both were misdiagnosed first and cost real time.

It also covers protocol surface `XMonad.River.WM` does not exercise. That list
has shrunk: `op_start_pointer`/`op_delta`/`op_release` is now wired up and
backs `mouseDrag`, `mouseMoveWindow` and `mouseResizeWindow`. Still unexercised
are `inform_resize_start`/`inform_resize_end`, edge-aware resize origin
correction, and `pointer_move_requested`/`pointer_resize_requested` — the
client-initiated half, which X11 had no equivalent of.

## 2. No runtime harness

Per the status line at the top of `README.river.md`, nothing has run against a
live river; everything above the wire codec is unverified.

The prototype's reusable asset is a recipe. Run river under
`WLR_BACKENDS=headless` with `-c <init-script>`, then assert on its debug log:

| Log line | Meaning |
| --- | --- |
| `manage sequence finish` | the manage sequence completed |
| `render sequence finish` | the render sequence completed |
| `window '…' mapped` | a client actually reached mapped state |
| `sent N tracked configure(s)`, N > 0 | **river accepted `propose_dimensions` and configured real windows** |

The last is the signal that matters — it is the only one that proves the layout
reached the compositor. No display, no hardware, CI-able.

Two findings no unit test would have caught:

- **Layer shell must be bound or river closes every layer surface on sight.**
  Already carried over — see the layer shell row in `README.river.md`. Worth
  knowing the symptom was silent: prompts simply never appeared, and the only evidence anywhere was one
  `info(wm)` line in river's log.
- **fuzzel does not exit when its layer surface is closed.** It hangs
  indefinitely rather than treating `closed` as fatal. See §3.

## 3. Blocking the event loop is an unguarded hazard — PARTLY CLOSED

A design constraint, not a bug report — relevant before this repo grows a
prompt or any other shell-out.

The event loop is single-threaded and is sole owner of both the connection and
`XState`. Any user action that blocks blocks everything: the loop stops reading
its socket, binding events queue unread, and the keyboard appears grabbed. There
is no escape hatch, because the escape hatch would itself be a binding. Recovery
is a TTY.

In the prototype this was real, not theoretical: the prompt shelled out to
fuzzel with `waitForProcess` from *inside the manage sequence*. Combined with
§2's hanging fuzzel, the WM wedged permanently. Running inside the manage
sequence compounds it — river's watchdog is held open for the whole prompt.

Design conclusions, which apply here unchanged:

- **Blocking actions do not need the manage sequence.**
  `windows` in `src-river/XMonad/Operations.hs` already requests
  `manage_dirty` when `inManageSeq` is false. Running actions after
  `manage_finish` costs one round trip and keeps the compositor healthy.
- **That alone does not fix the wedge.** The structural fix is for the loop
  never to block: fork the child, return immediately, hand the continuation
  back when it exits. Viable because the API a config uses is `mkXPrompt`'s
  *continuation* form; only `mkXPromptWithReturn` is inherently synchronous.
- **Two things that fix needs**, and why it is not small: a thread-safe way to
  hand an `X ()` back to the loop, and a wakeup — a self-pipe in `dispatch`'s
  wait set — so a result landing while the loop is parked in `recv` is not
  delayed until the next unrelated Wayland event. A background thread must
  never touch the connection directly; it is `IORef`-buffered and not
  thread-safe.

  **Both now exist.** `XMonad.River.Mailbox` is the queue and the self-pipe,
  `XMonad.River.postAction` is the way in, and the loop waits on the socket and
  the mailbox together. `XMonad.Util.Timer` is built on it, which is what
  proved the shape. What remains is the *use*: nothing yet runs a blocking
  action off the loop, so a prompt that shells out still wedges it.
- **A timeout was tried and rejected.** It bounds the damage without preventing
  it, and the bound is on the user thinking, not on anything mechanical.

## 4. CI never builds the river backend — CLOSED

Closed by `.github/workflows/river.yml`, which builds both backends with
`-f pedantic` and runs all four consistency scripts plus the `river-wire`
suite. Kept out of the Stack resolver matrix deliberately: the checks compare
the two backends against each other, so they need both halves built in one job.

Building with `-f pedantic` immediately found a real partial function —
`head initialWorkspaces` in `riverMain`, which crashed on a config with
`workspaces = []`.

What it looked like before:


So all 6,894 lines under `src-river/` are never compiled in CI, and the
`river-wire` suite never runs — its stanza in `xmonad.cabal` is
`buildable: False` unless the flag is on.

Separately, **none of the four consistency scripts runs anywhere**:
`tests/check-copies.sh`, `tests/check-x11-source.sh`, `tests/api/check-api.sh`
(twice, once per backend) and `tests/api/check-subset.sh`. The rebase-safety and API-subset apparatus is this
repo's best idea, and today it only protects the tree when someone remembers to
run it by hand.

## 5. Session integration is undocumented — CLOSED

Closed by [`INSTALL.river.md`](INSTALL.river.md). `INSTALL.md` remains the
untouched upstream X11 document, as it should — `src/` is frozen and so is its
documentation.

Each of the following cost real debugging time, none is discoverable from the
code, and all of them now have a home. They are kept here as the record of
*why* that document says what it says.

**Blank screen, cursor only, nothing in any log.** river's init exec'd a WM
binary that was never installed. Unlike X11 there is no free install step:
xmonad's recompile machinery calls `~/.xmonad/build`, but river just execs a
path. river runs happily with no window management client, so the failure is
invisible from the seat. *Fix:* an install step, plus an init that checks the
binary is executable and falls back to a terminal rather than leaving an
undebuggable session.

**Session exits instantly, empty log.** The session script sourced `~/.profile`
under `set -u`. `~/.profile` reads `HIDPI`, `DISPLAY` and `XDG_DATA_DIRS`
unguarded — harmless under the plain `/bin/sh` GDM's Xsession uses, fatal under
`-u`. It died before the logging block was reached, so GDM bounced back to the
login screen with nothing written. *Fix:* `set +u` around the sourcing.

**Half the startup applications silently missing.** GDM sources `~/.profile`
for X11 sessions via `/etc/gdm3/Xsession:38` but **not** for Wayland ones — it
execs the desktop entry directly. The session inherits only systemd's bare
PATH, missing `~/.local/bin`, `~/.cargo/bin`, `~/env/bin`.

**`graphical-session.target` refuses to start.** It sets `RefuseManualStart`
and may only be pulled in by dependency. *Fix:* a `river-session.target` that
`BindsTo` it.
