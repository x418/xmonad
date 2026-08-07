# Gaps carried over from the prototype

This repo is a rewrite of a working prototype (`~/env/xmonad-river` +
`~/env/xmonad-river-contrib`). The rewrite is better engineered — frozen X11
tree, tracked `patches/`, API goldens, no inert exports — but it was written
from the prototype's *design*, not its *operational experience*. The prototype
has since run against a live river, headless and on real hardware.

This records what that turned up. Scope note: the keybinding vocabulary gap —
no config being able to name `mod4Mask` or any keysym — was being closed while
this was written (`XMonad.Core` now exports the modifier masks, and
`XMonad.River.Keysym` the keysym table and `stringToKeysym`), so it is not
covered. Neither is the absent contrib layer, which is known and deferred.

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

It also covers protocol surface `XMonad.River.WM` does not exercise: the full
`op_start_pointer`/`op_delta`/`op_release` cycle,
`inform_resize_start`/`inform_resize_end`, edge-aware resize origin correction,
and `pointer_move_requested`/`pointer_resize_requested`.

## 2. No runtime harness

Per `README.river.md:8`, nothing has run against a live river; everything above
the wire codec is unverified.

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
  Already carried over (`README.river.md:114`). Worth knowing the symptom was
  silent: prompts simply never appeared, and the only evidence anywhere was one
  `info(wm)` line in river's log.
- **fuzzel does not exit when its layer surface is closed.** It hangs
  indefinitely rather than treating `closed` as fatal. See §3.

## 3. Blocking the event loop is an unguarded hazard

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
  `src-river/XMonad/Operations.hs:101-114` — `windows` already requests
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
- **A timeout was tried and rejected.** It bounds the damage without preventing
  it, and the bound is on the user thinking, not on anything mechanical.

## 4. CI never builds the river backend

- `.github/workflows/stack.yml:79` — `--flag=xmonad:pedantic`, no river flag.
- `.github/workflows/haskell-ci.yml` — zero occurrences of "river".
- `cabal.haskell-ci` — `+pedantic` only.

So all 6,753 lines under `src-river/` are never compiled in CI, and the
`river-wire` suite never runs (`xmonad.cabal:212`, `buildable: False` unless
the flag is on).

Separately, **none of the four consistency scripts runs anywhere**:
`tests/check-copies.sh`, `tests/check-x11-source.sh`, `tests/api/check-api.sh`,
`tests/api/check-subset.sh`. The rebase-safety and API-subset apparatus is this
repo's best idea, and today it only protects the tree when someone remembers to
run it by hand.

## 5. Session integration is undocumented

`INSTALL.md` has zero river content — it is the untouched upstream X11 document
and covers only `.desktop` files for X sessions. Each of the following cost real
debugging time, and none is discoverable from the code.

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
