#!/usr/bin/env bash
#
# Assert that fullscreen reaches river and comes back, against a real
# compositor.
#
# Usage: tests/headless-fullscreen.sh
#
# A client asking for fullscreen is owed a configure whatever the window
# manager decides, and one that never gets it waits: a browser whose video
# froze mid-transition, with nothing in any log.  The window manager is
# tests/river-fullscreen-wm.hs, which grants fullscreen with setFullscreen and
# then, on a timer, takes it back, grants it again and closes the window.
# river's protocol log (WAYLAND_DEBUG=server) is the oracle:
#
#   fullscreen(river_output_v1#M)  the window was handed to river
#   inform_fullscreen()            and told
#   exit_fullscreen, inform_not_fullscreen, propose_dimensions, set_position
#                                  all in one manage sequence, which the
#                                  protocol asks for: river leaves position and
#                                  size undefined until both arrive
#   configure(W/2, H/2, ...)       the float it left to, the middle quarter
#   configure(W, H, ...)           after the second fullscreen, at the
#                                  output's size, which only river's
#                                  fullscreen produces from that float
#
# and the window manager's own log says the event hook heard the window close
# and the backend let it go.

set -uo pipefail

cd "$(dirname "$0")/.."

if ! command -v river >/dev/null; then
    echo "headless-fullscreen: river is not installed; skipping" >&2
    exit 77   # automake's "skipped" convention
fi

# A client that asks for fullscreen before its first commit.
if command -v foot >/dev/null; then
    CLIENT=(foot --fullscreen)
elif command -v alacritty >/dev/null; then
    CLIENT=(alacritty -o 'window.startup_mode="Fullscreen"')
else
    echo "headless-fullscreen: neither foot nor alacritty is installed; skipping" >&2
    exit 77
fi

WM=${XMONAD_RIVER_FULLSCREEN_WM:-}
[ -n "$WM" ] || WM=$(find .stack-work/install -path '*9.10*' -name river-fullscreen-wm -type f -perm -u+x 2>/dev/null | head -1)
if [ -z "$WM" ]; then
    echo "headless-fullscreen: no river-fullscreen-wm build found; run" >&2
    echo "  stack build" >&2
    exit 1
fi
case "$WM" in /*) ;; *) WM=$PWD/$WM ;; esac

# Short, for the sockaddr_un limit; see headless-river.sh.
RT=$(mktemp -d /tmp/xf.XXXXXX)
chmod 700 "$RT"
LOG=$RT/river.log
WMLOG=$RT/wm.log
ACTIONS=$RT/actions
trap 'rm -rf "$RT"' EXIT

# A data directory of its own, so this cannot consume the resume state of a
# window manager running for real on this machine.
DATA=$RT/data
mkdir -p "$DATA"

printf -v client '%q ' "${CLIENT[@]}"
cat > "$RT/init.sh" <<EOF
#!/bin/sh
XMONAD_FULLSCREEN_LOG="$ACTIONS" XMONAD_DATA_DIR="$DATA" "$WM" > "$WMLOG" 2>&1 &
sleep 2
$client > "$RT/client.log" 2>&1 &
# The window manager's schedule takes fifteen seconds from its start.
sleep 18
EOF
chmod +x "$RT/init.sh"

echo "headless-fullscreen: wm=$WM"
echo "headless-fullscreen: client=${CLIENT[*]}"

timeout 30 env \
    XDG_RUNTIME_DIR="$RT" \
    WLR_BACKENDS=headless \
    WLR_LIBINPUT_NO_DEVICES=1 \
    WAYLAND_DEBUG=server \
    river -log-level debug -no-xwayland -c "$RT/init.sh" > "$LOG" 2>&1

status=0
pass() { echo "  PASS  $1"; }
fail() { echo "  FAIL  $1" >&2; status=1; }

echo
echo "headless-fullscreen: results"

if ! grep -qx 'requested True' "$ACTIONS" 2>/dev/null; then
    fail "the client asked for fullscreen and the event hook heard it"
    echo "--- wm log ---" >&2
    tail -25 "$WMLOG" >&2
    echo "--- client log ---" >&2
    tail -10 "$RT/client.log" >&2
    exit 1
fi
pass "the client asked for fullscreen and the event hook heard it"

# The window the client opened: the one river forwarded the request for.
win=$(grep -oE 'river_window_v1#[0-9]+\.fullscreen_requested' "$LOG" | head -1 | cut -d. -f1)
node=$(grep -oE "${win}\.get_node\(new id river_node_v1#[0-9]+\)" "$LOG" | head -1 | grep -oE 'river_node_v1#[0-9]+')
toplevel=$(grep -oE 'xdg_toplevel#[0-9]+\.set_fullscreen' "$LOG" | head -1 | cut -d. -f1)
read -r ow oh < <(grep -oE 'river_output_v1#[0-9]+\.dimensions\([0-9]+, [0-9]+\)' "$LOG" \
                  | head -1 | grep -oE '[0-9]+, [0-9]+' | tr -d ,)
echo "  ....  window=$win node=$node toplevel=$toplevel output=${ow}x${oh}"

entered=$(grep -cE "${win}\.fullscreen\(river_output_v1#[0-9]+\)" "$LOG")
[ "$entered" -ge 2 ] \
    && pass "fullscreen was sent, and sent again after leaving ($entered)" \
    || fail "fullscreen was sent, and sent again after leaving ($entered, want 2)"

grep -qE "${win}\.inform_fullscreen\(\)" "$LOG" \
    && pass "the client was told it is fullscreen" \
    || fail "the client was told it is fullscreen"

grep -qE "${toplevel}\.configure\(${ow}, ${oh}," "$LOG" \
    && pass "the client was configured at the output's size" \
    || fail "the client was configured at the output's size"

# In order: the quarter float it left to, then the output again.
sizes=$(awk -v w="$win" -v t="$toplevel" \
            -v small="($((ow / 2)), $((oh / 2))," -v big="(${ow}, ${oh}," '
    index($0, w ".exit_fullscreen()")                 { left = 1 }
    left && index($0, t ".configure" small)           { shrunk = 1 }
    shrunk && index($0, w ".fullscreen(")             { back = 1 }
    back && index($0, t ".configure" big)             { grown = 1 }
    END { print (shrunk + 0) " " (grown + 0) }' "$LOG")
[ "${sizes% *}" = 1 ] \
    && pass "leaving gave the client the layout's size" \
    || fail "leaving gave the client the layout's size"
[ "${sizes#* }" = 1 ] \
    && pass "going back gave it the output's size again" \
    || fail "going back gave it the output's size again"

# Leaving: everything river needs to place the window again, in one sequence.
together=$(awk -v w="$win" -v n="$node" '
    /manage_start\(\)/ { e = i = p = s = 0 }
    index($0, w ".exit_fullscreen()")       { e = 1 }
    index($0, w ".inform_not_fullscreen()") { i = 1 }
    index($0, w ".propose_dimensions(")     { p = 1 }
    index($0, n ".set_position(")           { s = 1 }
    /manage_finish\(\)/ { if (e && i && p && s) found = 1 }
    END { print found + 0 }' "$LOG")
[ "$together" = 1 ] \
    && pass "leaving sent exit, inform, dimensions and position in one manage sequence" \
    || fail "leaving sent exit, inform, dimensions and position in one manage sequence"

grep -qx 'destroyed' "$ACTIONS" \
    && pass "closing the window reached the event hook" \
    || fail "closing the window reached the event hook"
grep -qx 'granted 0' "$ACTIONS" \
    && pass "a window closed while fullscreen was let go" \
    || fail "a window closed while fullscreen was let go ($(grep '^granted' "$ACTIONS"))"

trouble=$(grep -nE '^river-fullscreen-wm:|protocol error|invalid object|no such interface' "$WMLOG" 2>/dev/null \
          | grep -vE 'river-fullscreen-wm: (requested|leave|enter|close|destroyed|granted)')
if [ -n "$trouble" ]; then
    fail "the window manager reported no errors"
    echo "$trouble" | head -10 | sed 's/^/    /' >&2
fi
if grep -qE 'error\(wm\)|protocol error' "$LOG"; then
    fail "the compositor reported an error"
    grep -E 'error\(wm\)|protocol error' "$LOG" | head -5 | sed 's/^/    /' >&2
fi

exit "$status"
