#!/usr/bin/env bash
#
# Run tests/river-prompt-spec.hs against a real compositor.
#
# Usage: tests/headless-prompt.sh
#
# The sibling of tests/headless-river.sh, and the same recipe -- headless
# wlroots, no display, no hardware -- pointed at a different question.  That
# script asks whether the window manager manages windows; this one asks whether
# a prompt can be relied on not to run off with the keyboard.
#
# Why it needs a compositor at all: everything worth checking here is a
# negotiation with one.  Whether a layer surface is configured, whether an
# exclusive keyboard grab is granted, whether a keymap arrives -- none of that
# can be faked, and all of it is what the startup watchdog in
# XMonad.River.Client decides on.
#
# Two things about the environment are not obvious:
#
#   * The window manager has to be running.  River closes any layer surface
#     whose namespace it does not recognise unless a window manager has bound
#     river_layer_shell_v1 -- "window manager did not bind river_layer_shell_v1,
#     closing layer surface" -- so a prompt with no xmonad behind it never maps
#     at all, and every assertion here would fail for a reason that has nothing
#     to do with prompts.
#
#   * A headless seat has no keyboard.  That is not a limitation to work
#     around, it is one of the cases the watchdog exists for, so the run
#     asserts that it fires and says so -- and asserts, separately, that it
#     leaves alone a surface that never wanted a keyboard.
#
# What this cannot check headlessly is the other false-positive: an idle prompt
# on a seat that does have a keyboard must not be closed.  That needs a
# keyboard capability, which means a virtual-keyboard-unstable-v1 client, which
# means bindings this repo does not generate.  See future-work.md.

set -uo pipefail

cd "$(dirname "$0")/.."

if ! command -v river >/dev/null; then
    echo "headless-prompt: river is not installed; skipping" >&2
    exit 77   # automake's "skipped" convention
fi

WM=${XMONAD_RIVER_WM:-}
[ -n "$WM" ] || WM=$(find .stack-work/install -path '*9.10*' -name xmonad -type f -perm -u+x 2>/dev/null | head -1)
if [ -z "$WM" ]; then
    echo "headless-prompt: no river build found; run" >&2
    echo "  stack build --flag xmonad:river" >&2
    exit 1
fi
case "$WM" in /*) ;; *) WM=$PWD/$WM ;; esac

SPEC=${XMONAD_RIVER_PROMPT_SPEC:-}
[ -n "$SPEC" ] || SPEC=$(find .stack-work/install -path '*9.10*' -name river-prompt-spec -type f -perm -u+x 2>/dev/null | head -1)
if [ -z "$SPEC" ]; then
    echo "headless-prompt: no river-prompt-spec build found; run" >&2
    echo "  stack build --flag xmonad:river" >&2
    exit 1
fi
case "$SPEC" in /*) ;; *) SPEC=$PWD/$SPEC ;; esac

# Short, for the sockaddr_un limit; see headless-river.sh.
RT=$(mktemp -d /tmp/xp.XXXXXX)
chmod 700 "$RT"
LOG=$RT/river.log
SPECLOG=$RT/spec.log
trap 'rm -rf "$RT"' EXIT

# The spec's exit status has to survive back out of river, which exits with its
# own.  A file is the simplest channel that does.
WMLOG=$RT/wm.log
cat > "$RT/init.sh" <<EOF
#!/bin/sh
# The window manager first, and given a moment to bind river_layer_shell_v1:
# without that river closes the spec's layer surfaces on sight.
XMONAD_RIVER_NO_STARTUP_HOOK=1 "$WM" > "$WMLOG" 2>&1 &
sleep 3
"$SPEC" > "$SPECLOG" 2>&1
echo \$? > "$RT/status"
EOF
chmod +x "$RT/init.sh"

echo "headless-prompt: wm=$WM"
echo "headless-prompt: spec=$SPEC"

timeout 120 env \
    XDG_RUNTIME_DIR="$RT" \
    WLR_BACKENDS=headless \
    WLR_LIBINPUT_NO_DEVICES=1 \
    river -log-level debug -no-xwayland -c "$RT/init.sh" > "$LOG" 2>&1

status=$(cat "$RT/status" 2>/dev/null || echo 1)

echo
echo "headless-prompt: results"
sed 's/^/  /' "$SPECLOG" 2>/dev/null | grep -E 'PASS|FAIL|note|skip' || {
    echo "  FAIL  the spec produced no output" >&2
    echo "--- river log tail ---" >&2
    tail -20 "$LOG" >&2
    exit 1
}

# river's own view, which is the independent oracle: it logs a layer surface
# being mapped and destroyed by namespace.  Every prompt this run opened must
# have been destroyed, or something is still holding the keyboard -- and that
# is true whatever the spec itself thought.
mapped=$(grep -c "layer surface 'xmonad-prompt' mapped" "$LOG")
destroyed=$(grep -c "layer surface 'xmonad-prompt' destroyed" "$LOG")
echo "  ....  river mapped $mapped prompt surface(s), destroyed $destroyed"
if [ "$mapped" -gt 0 ] && [ "$mapped" -ne "$destroyed" ]; then
    echo "  FAIL  a prompt surface outlived the run" >&2
    status=1
fi

# The watchdog's own diagnostic, echoed so a run that fails is readable without
# digging.  On a headless seat it is expected, and naming the seat is the point:
# a message that said only "unusable" would not tell anyone what to fix.
grep -h 'never became usable' "$SPECLOG" 2>/dev/null | sed 's/^/  ....  /' | head -4

exit "$status"
