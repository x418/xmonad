# Gaps carried over from the prototype

This repo is a rewrite of a working prototype. The rewrite is better
engineered — frozen X11 tree, tracked `patches/`, API goldens, no inert
exports — but it was written from the prototype's *design*, not its
*operational experience*. The prototype had since run against a live river,
headless and on real hardware.

This is what that turned up and what is left of it. Closed items are deleted
rather than marked, because a document of things that are fine is a document
nobody reads; the reasoning behind each fix lives next to the code that
implements it, and `git log` has the rest.

For contrib, see `../xmonad-river-contrib`: `SURVEY.md` for what compiles —
283 of 334 — and `future-work.md` §6 for why each of the remaining 25 does
not.

## 1. No reference window manager

`app/tinyrwm/Main.hs` in the prototype — 419 lines, a port of river's own
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

## 2. A blocking action still wedges the event loop

The event loop is single-threaded and is sole owner of both the connection and
`XState`. Any user action that blocks blocks everything: the loop stops reading
its socket, binding events queue unread, and the keyboard appears grabbed.

The machinery to avoid that exists and is used. `XMonad.River.Mailbox` is a
queue with a self-pipe in `dispatch`'s wait set, so a result landing while the
loop is parked in `recv` is not delayed until the next unrelated Wayland event;
`XMonad.River.postAction` is the way in. `XMonad.Util.Timer`, `XMonad.Prompt`
and `XMonad.Actions.GridSelect` are all built on it, which is what proved the
shape.

**What remains is everything that shells out.** A config action that runs a
subprocess and waits for it still blocks the loop, and there is no escape hatch
short of `XMonad.River.closeAllPrompts` — which only helps if what is stuck is
a prompt. The structural fix is the same one the prompt got: fork the child,
return immediately, hand the continuation back when it exits.

In the prototype this was not theoretical. The prompt shelled out to fuzzel
with `waitForProcess` from *inside the manage sequence*, and fuzzel does not
exit when its layer surface is closed — it hangs rather than treating `closed`
as fatal — so the WM wedged permanently, with river's watchdog held open for
the duration. Running such an action after `manage_finish` rather than inside
the sequence costs one round trip and keeps the compositor healthy; `windows`
already requests `manage_dirty` when `inManageSeq` is false.

A timeout was tried and rejected. It bounds the damage without preventing it,
and the bound is on the user thinking rather than on anything mechanical.
