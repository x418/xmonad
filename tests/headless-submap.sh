#!/usr/bin/env bash
#
# Assert that a submap gives the config's bindings back, against a real
# compositor.
#
# Usage: tests/headless-submap.sh
#
# The question this asks is narrow and the consequence of getting it wrong is
# not: a submap disables every one of the window manager's bindings while it is
# open, so a teardown that does not run leaves a session in which no shortcut
# works.  There is no way to notice that from inside -- the window manager is
# fine, it simply has no bindings -- and no other test in the suite covers it.
#
# The run is tests/river-submap-spec.hs, which is a window manager rather than
# a client: a submap is made of bindings, and only the window manager has any.
# It gives the seat a keyboard through virtual-keyboard-unstable-v1, because a
# headless seat has none, and then presses:
#
#     M-m   opens a submap                      (globals now disabled)
#     a     is in it                            -> "inner-a", teardown
#     M-b   an ordinary global binding          -> "global-b"
#     M-m   opens it again
#     z     is not in it                        -> "inner-unbound", teardown
#     M-b   again                               -> "global-b"
#
# The two "global-b" lines are the whole point.  Either one missing means the
# bindings a submap disabled were not restored.

set -uo pipefail

cd "$(dirname "$0")/.."

if ! command -v river >/dev/null; then
    echo "headless-submap: river is not installed; skipping" >&2
    exit 77   # automake's "skipped" convention
fi

SPEC=${XMONAD_RIVER_SUBMAP_SPEC:-}
[ -n "$SPEC" ] || SPEC=$(find .stack-work/install -path '*9.10*' -name river-submap-spec -type f -perm -u+x 2>/dev/null | head -1)
if [ -z "$SPEC" ]; then
    echo "headless-submap: no river-submap-spec build found; run" >&2
    echo "  stack build" >&2
    exit 1
fi
case "$SPEC" in /*) ;; *) SPEC=$PWD/$SPEC ;; esac

# Short, for the sockaddr_un limit; see headless-river.sh.
RT=$(mktemp -d /tmp/xs.XXXXXX)
chmod 700 "$RT"
LOG=$RT/river.log
SPECLOG=$RT/spec.log
ACTIONS=$RT/actions
trap 'rm -rf "$RT"' EXIT

# A data directory of its own, so this cannot consume the resume state of a
# window manager running for real on this machine.
DATA=$RT/data
mkdir -p "$DATA"

cat > "$RT/init.sh" <<EOF
#!/bin/sh
XMONAD_SUBMAP_LOG="$ACTIONS" XMONAD_DATA_DIR="$DATA" \
    XMONAD_RIVER_NO_STARTUP_HOOK=1 "$SPEC" > "$SPECLOG" 2>&1 &
# Long enough for the spec's own keyboard schedule to finish; it sleeps 4s
# before it starts and about 1.2s between presses.
sleep 20
EOF
chmod +x "$RT/init.sh"

echo "headless-submap: spec=$SPEC"

timeout 90 env \
    XDG_RUNTIME_DIR="$RT" \
    WLR_BACKENDS=headless \
    WLR_LIBINPUT_NO_DEVICES=1 \
    river -log-level debug -no-xwayland -c "$RT/init.sh" > "$LOG" 2>&1

status=0
pass() { echo "  PASS  $1"; }
fail() { echo "  FAIL  $1" >&2; status=1; }

echo
echo "headless-submap: results"

if [ ! -s "$ACTIONS" ]; then
    fail "no binding ran at all"
    echo "--- spec log ---" >&2
    tail -25 "$SPECLOG" >&2
    echo "--- river log tail ---" >&2
    tail -25 "$LOG" >&2
    exit 1
fi

got=$(tr '\n' ' ' < "$ACTIONS" | sed 's/ *$//')
echo "  ....  actions: $got"

# The submap ran the key it was given, rather than the global bound to it.
grep -qx 'inner-a' "$ACTIONS" \
    && pass "a submap key runs the submap's action" \
    || fail "a submap key runs the submap's action"

# A key the submap did not want ends it, rather than leaving it armed.
grep -qx 'inner-unbound' "$ACTIONS" \
    && pass "an unknown key ends the submap" \
    || fail "an unknown key ends the submap"

# The assertion this test exists for, twice: once after each way out.
globals=$(grep -cx 'global-b' "$ACTIONS")
if [ "$globals" -ge 2 ]; then
    pass "the config's bindings come back after a submap ($globals/2)"
else
    fail "the config's bindings come back after a submap ($globals/2)"
fi

# Ordering, which the counts alone would not catch: a global-b before any
# submap ran would mean the submap never armed and M-m did nothing.
first=$(head -1 "$ACTIONS")
[ "$first" = "inner-a" ] \
    && pass "the submap armed before the first global fired" \
    || fail "the submap armed before the first global fired (first was '$first')"

# Anchored the way headless-river.sh anchors its own check, and for the same
# reason: a bare grep for "error" matches wlroots printing its EGL extension
# list, which contains EGL_EXT_create_context_robustness among others.
trouble=$(grep -nE '^river-submap-spec:|protocol error|invalid object|no such interface' "$SPECLOG" 2>/dev/null \
          | grep -vE 'river-submap-spec: (inner-|global-)')
if [ -n "$trouble" ]; then
    fail "the window manager reported no errors"
    echo "$trouble" | head -10 | sed 's/^/    /' >&2
fi
if grep -qE 'error\(wm\)|error\(input\)|protocol error' "$LOG"; then
    fail "the compositor reported an error"
    grep -E 'error\(wm\)|error\(input\)|protocol error' "$LOG" | head -5 | sed 's/^/    /' >&2
fi

exit "$status"
