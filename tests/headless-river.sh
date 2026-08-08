#!/usr/bin/env bash
#
# Run the river backend against a real compositor, with no display and no
# hardware, and assert on what river says happened.
#
# Usage: tests/headless-river.sh [seconds]
#
# Everything above the wire codec was unverified until this existed: the
# protocol stack, the manage/render loop, the event loop, all of it compiled
# and none of it had run.  The first few runs found three bugs that no amount of
# compiling would have (see Findings at the bottom), which is the argument for
# the script.
#
# == The recipe
#
# river speaks to wlroots, and wlroots will use a headless backend if asked, so
# no seat, no GPU and no display are required.  The window manager is an
# ordinary client of it, started from river's init.
#
# Three things about the environment are not obvious and each cost a run:
#
#   * XDG_RUNTIME_DIR must be short.  The Wayland socket path goes in a
#     sockaddr_un, which caps out at 108 bytes; a runtime dir under a deep
#     temporary path silently exhausts every socket name from wayland-0 to
#     wayland-32 and then river exits with AddSocketFailed.
#
#   * -no-xwayland, or river tries to start Xwayland and fails the whole
#     session when it cannot.
#
#   * Subprocess output must go to a file rather than through a pipe.  Anything
#     buffering in a pipeline loses its output when the run is killed, which
#     reads as "the window manager printed nothing" -- and "printed nothing" is
#     also what a healthy window manager does.
#
# == What is asserted
#
# river's debug log is the oracle.  The line that matters is the last one:
#
#   manage sequence finish        a manage sequence completed
#   render sequence finish        a render sequence completed
#   new xdg_toplevel              a client actually asked for a window
#   sent N tracked configure(s)   with N > 0, river accepted propose_dimensions
#                                 and configured a real window
#
# Only the last proves the layout reached the compositor.  The others are true
# of a river with no window manager at all, which is exactly the trap: river
# runs manage and render sequences on its own schedule regardless, so counting
# them proves nothing.

set -uo pipefail

cd "$(dirname "$0")/.."

DURATION=${1:-12}

if ! command -v river >/dev/null; then
    echo "headless-river: river is not installed; skipping" >&2
    exit 77   # automake's "skipped" convention
fi

# A client to open a window with.  Any Wayland client will do; the test only
# needs something that creates an xdg_toplevel.
CLIENT=""
for c in foot alacritty kitty weston-terminal; do
    command -v "$c" >/dev/null && { CLIENT=$c; break; }
done

# A config of your own can be tested by pointing this at its binary, which is
# how xmonad-contrib gets exercised: build a config that imports the modules
# you care about and run it here.
WM=${XMONAD_RIVER_WM:-}
[ -n "$WM" ] || WM=$(find .stack-work/install -path '*9.10*' -name xmonad -type f -perm -u+x 2>/dev/null | head -1)
if [ -z "$WM" ]; then
    echo "headless-river: no river build found; run" >&2
    echo "  stack build" >&2
    exit 1
fi
case "$WM" in /*) ;; *) WM=$PWD/$WM ;; esac

# Short, for the sockaddr_un limit above.
RT=$(mktemp -d /tmp/xr.XXXXXX)
chmod 700 "$RT"
LOG=$RT/river.log
WMLOG=$RT/wm.log
trap 'rm -rf "$RT"' EXIT

cat > "$RT/init.sh" <<EOF
#!/bin/sh
# river runs this instead of the default init.  It starts the window manager,
# which connects back as a client, then a client to give it something to do.
"$WM" > "$WMLOG" 2>&1 &
sleep 3
${CLIENT:+$CLIENT >> "$WMLOG" 2>&1 &}
sleep 3
${CLIENT:+$CLIENT >> "$WMLOG" 2>&1 &}
sleep $DURATION
EOF
chmod +x "$RT/init.sh"

echo "headless-river: wm=$WM"
echo "headless-river: client=${CLIENT:-<none found>}"

# The startup hook is suppressed unless asked for, because a real config's
# startup hook is the one part of it that reaches outside its own session.  The
# config this was first run against spawns a dozen processes through systemd-cat
# -- so they outlive the compositor rather than dying with it -- and kills tmux
# sessions by name, which would have taken out the live ones had they existed.
# Nothing being tested here needs it.
#
#   WITH_STARTUP_HOOK=1 tests/headless-river.sh
#
# runs it anyway, for when the hook itself is what you are testing.
STARTUP_ENV=(XMONAD_RIVER_NO_STARTUP_HOOK=1)
if [ -n "${WITH_STARTUP_HOOK:-}" ]; then
    STARTUP_ENV=()
    echo "headless-river: WARNING: running the config's startup hook; it may" >&2
    echo "                spawn processes and alter the live session" >&2
fi

timeout $((DURATION + 20)) env \
    XDG_RUNTIME_DIR="$RT" \
    WLR_BACKENDS=headless \
    WLR_LIBINPUT_NO_DEVICES=1 \
    ${STARTUP_ENV[@]+"${STARTUP_ENV[@]}"} \
    river -log-level debug -no-xwayland -c "$RT/init.sh" > "$LOG" 2>&1

configures=$(grep -oE 'sent [0-9]+ tracked configure' "$LOG" \
             | grep -oE '[0-9]+' | sort -rn | head -1)
configures=${configures:-0}
toplevels=$(grep -c 'new xdg_toplevel' "$LOG")
manages=$(grep -c 'manage sequence finish' "$LOG")

status=0
report() {
    if [ "$2" = ok ]; then printf '  PASS  %s\n' "$1"
    else printf '  FAIL  %s\n' "$1" >&2; status=1
    fi
}

echo
echo "headless-river: results"
[ "$manages" -gt 0 ] && report "river completed a manage sequence" ok \
                     || report "river completed a manage sequence" no

# Only backend-level trouble counts.  "Printed nothing at all" was the right
# test while the only thing run here was a bare test binary, and it stopped
# being right the moment XMONAD_RIVER_WM was pointed at a real config: a
# startup hook that spawns a dozen processes and logs what it spawned is
# working, not failing, and several of those processes legitimately fail in a
# sandbox with no session to attach to.
#
# So the assertion names what actually indicates a broken backend.  The
# alternative -- keeping "no output" and telling people to ignore it -- is an
# assertion nobody reads.
# Anchored, because a config's binary is very likely to be called something
# ending in "xmonad-river" and its own spawn failures would match otherwise --
# which is exactly what happened the first time this ran against a real config.
# "xmonad-river: note: ..." is the backend saying something worth knowing, not
# something going wrong; anything else it prints about itself is a problem.
backend_trouble=$(grep -nE '^xmonad-river:|protocol error|wl_display|invalid object|no such interface' "$WMLOG" 2>/dev/null \
                  | grep -v '^[0-9]*:xmonad-river: note:')
if [ -n "$backend_trouble" ]; then
    report "the backend reported no errors" no
    echo "--- backend errors ---" >&2
    echo "$backend_trouble" | head -20 >&2
else
    report "the backend reported no errors" ok
fi

# Not an assertion.  A config's own output is expected, but it is the first
# thing wanted when something else fails, so it is shown rather than hidden.
if [ -s "$WMLOG" ]; then
    echo
    echo "window manager output ($(wc -l < "$WMLOG") lines, first 15):"
    head -15 "$WMLOG" | sed 's/^/  | /'
fi

if [ -n "$CLIENT" ]; then
    [ "$toplevels" -gt 0 ] && report "a client created a toplevel" ok \
                           || report "a client created a toplevel" no
    # The one that matters.
    [ "$configures" -gt 0 ] \
        && report "river configured a window from the layout ($configures)" ok \
        || report "river configured a window from the layout (got 0)" no
    # Two windows configured in one sequence is the layout tiling rather than
    # merely placing: both were given geometry from the same run of it.
    [ "$configures" -gt 1 ] \
        && report "the layout tiled two windows at once ($configures)" ok \
        || report "the layout tiled two windows at once (got $configures)" no
fi

if [ "$status" -ne 0 ]; then
    echo >&2
    echo "full river log: $LOG (removed on exit; rerun with the trap disabled to keep it)" >&2
fi
exit "$status"

# == Findings
#
# Three bugs, all invisible to the type checker and to every unit test:
#
#   * recvWithFds died with "resource exhausted".  GHC's IO manager puts every
#     socket it owns into non-blocking mode, so an empty socket answers EAGAIN
#     rather than waiting.  The C shim's caller retried EINTR but treated
#     EAGAIN as fatal -- the network package's recv handles this internally,
#     and hand-rolling recvmsg lost it.  Fixed by waiting for readability, as
#     the IO manager would.
#
#   * The event loop ran exactly one iteration.  Swapping the blocking read for
#     a two-source wait dropped the recursive tail call, so the window manager
#     connected, handled one event and returned.  It stayed alive and silent,
#     which looks identical to working.
#
#   * The loop waited before it flushed.  manage_dirty is queued rather than
#     written, so the very first pass waited for a reply to a request that had
#     never left the buffer, and every later one was a round trip late.  The
#     loop this replaced could not get it wrong: dispatch flushes before it
#     reads.  This was the one keeping every window unconfigured.
#
# The executable also needed -threaded, which the design had always assumed:
# the loop forks a watcher per descriptor, sendRestart interrupts a blocking
# read with an async exception, and prompts run on threads of their own.
#
# With those four fixed, all four assertions pass.
