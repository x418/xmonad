## Default terminal

Looks like claude chose "foot"

## Revisit CI setup

Shouldn't be so gnarly

## My idiosyncratic patches

Should resolve. Was sendRestart exposed before?

## Assert that some modules are identical

## TODO

**Nothing tests the loop.** The suites cover `XMonad.StackSet` and the wire
codec. The concurrency invariants — no torn reads, ops delivered exactly once,
liveness filtering complete — are precisely what property tests cannot reach.
`tests/headless-river.sh` and `tests/headless-prompt.sh` are the only harnesses
that run against a real compositor, and they would need extending.

**The shared-`IORef` audit is the real surface area of that change.** `XConf`
carries about eleven of them and each needs an owner. `rtBindings` and
`rtSubmap` are the awkward pair: they are deliberately shared between the IO
callbacks and `X` code today, which is exactly the sharing the split breaks.

## Rename closeAllPrompts

Utility to generically interrupt the worker?

## A blocking action wedges the event loop

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
