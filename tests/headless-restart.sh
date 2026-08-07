#!/usr/bin/env bash
#
# Assert that window state survives a restart, against a real compositor.
#
# Usage: tests/headless-restart.sh
#
# == Why this needs a compositor
#
# The interesting claim is not that a StackSet can be serialised -- that is
# ordinary Haskell -- but that river_window_v1.identifier means the same thing
# to the window manager that reads the state file as it did to the one that
# wrote it.  Nothing below the compositor can answer that.  River's own source
# says it should hold:
#
#   * the identifier comes from the window's ext_foreign_toplevel_handle_v1,
#     which belongs to the window rather than to the window manager's
#     connection, and
#   * Window.zig creates a handle only "if the window manager is restarted"
#     has not already left one in place.
#
# Reading that is not the same as observing it, and a compositor upgrade could
# change it without anything here failing to compile.
#
# The ordering question is settled the same way.  A restore is only possible if
# every window's identifier has arrived before the first manage_start;
# WindowManager.manageStart() iterates the windows, sending window and
# identifier for each, and only then sends manage_start.  If that ever changes,
# this test reports a partial restore rather than a total one.
#
# == The assertion
#
# The window manager prints, on the way through its first manage sequence:
#
#   xmonad-river: note: restored N of M windows from <path>
#
# M is what the state file claimed, N is how many identifiers resolved to a
# window river is currently reporting.  N == M > 0 is the pass.  N < M is the
# failure this exists to catch, and is exactly what a naive implementation
# keyed on object ids would produce: the file written, read back, and every
# window in it silently dropped.
#
# See tests/headless-river.sh for the environment notes (short XDG_RUNTIME_DIR,
# -no-xwayland, output to files rather than pipes); they apply here too.

set -uo pipefail

cd "$(dirname "$0")/.."

if ! command -v river >/dev/null; then
    echo "headless-restart: river is not installed; skipping" >&2
    exit 77
fi

CLIENT=""
for c in foot alacritty kitty weston-terminal; do
    command -v "$c" >/dev/null && { CLIENT=$c; break; }
done
if [ -z "$CLIENT" ]; then
    echo "headless-restart: no Wayland terminal found; skipping" >&2
    exit 77
fi

WM=${XMONAD_RIVER_WM:-}
[ -n "$WM" ] || WM=$(find .stack-work/install -path '*9.10*' -name xmonad -type f -perm -u+x 2>/dev/null | head -1)
if [ -z "$WM" ]; then
    echo "headless-restart: no river build found; run" >&2
    echo "  stack build --flag xmonad:river" >&2
    exit 1
fi
case "$WM" in /*) ;; *) WM=$PWD/$WM ;; esac

RT=$(mktemp -d /tmp/xr.XXXXXX)
chmod 700 "$RT"
LOG=$RT/river.log
WMLOG=$RT/wm.log
PSLOG=$RT/ps.log
trap 'rm -rf "$RT"' EXIT

# A data directory of our own, so the state file and the pid file cannot touch
# the ones belonging to a window manager running for real on this machine.
# Getting this wrong would have the test consume a live session's resume state.
DATA=$RT/data
mkdir -p "$DATA"

cat > "$RT/init.sh" <<EOF
#!/bin/sh
"$WM" > "$WMLOG" 2>&1 &
sleep 3
$CLIENT >> "$WMLOG" 2>&1 &
sleep 2
$CLIENT >> "$WMLOG" 2>&1 &
sleep 4
# The restart, sent the way a separate process has to send it.  Going through
# the pid file rather than pkill is deliberate: it exercises the path that
# 'xmonad --restart' takes, which was calling sendRestart in the wrong process
# and doing nothing at all.
"$WM" --restart >> "$WMLOG" 2>&1
sleep 6
# Twice, because one restart cannot tell a restore apart from a restore that
# also re-managed everything it restored.  A window that goes through the
# manage hook again is inserted a second time, so the next state file records
# four windows instead of two -- and the counts below catch it.
"$WM" --restart >> "$WMLOG" 2>&1
sleep 6
# What the window manager's process tree looks like once the dust settles.
# A restart that goes through a shell leaves one behind -- sh -c execs only in
# the narrowest cases and otherwise forks and waits -- and each restart
# inherits the last one's, so they nest one per M-q.
ps -o args= -u "\$(id -u)" > "$PSLOG" 2>/dev/null
EOF
chmod +x "$RT/init.sh"

echo "headless-restart: wm=$WM"
echo "headless-restart: client=$CLIENT"

timeout 60 env \
    XDG_RUNTIME_DIR="$RT" \
    XMONAD_DATA_DIR="$DATA" \
    XMONAD_CONFIG_DIR="$DATA" \
    XMONAD_CACHE_DIR="$DATA" \
    XMONAD_RIVER_NO_STARTUP_HOOK=1 \
    WLR_BACKENDS=headless \
    WLR_LIBINPUT_NO_DEVICES=1 \
    river -log-level debug -no-xwayland -c "$RT/init.sh" > "$LOG" 2>&1

status=0
report() {
    if [ "$2" = ok ]; then printf '  PASS  %s\n' "$1"
    else printf '  FAIL  %s\n' "$1" >&2; status=1
    fi
}

echo
echo "headless-restart: results"

mapfile -t restores < <(grep -oE 'restored [0-9]+ of [0-9]+ windows' "$WMLOG")
if [ "${#restores[@]}" -ne 2 ]; then
    report "both successors read a state file (got ${#restores[@]} of 2)" no
else
    report "both successors read a state file" ok
    for r in "${restores[@]}"; do
        got=$(echo "$r" | awk '{print $2}')
        want=$(echo "$r" | awk '{print $4}')
        # The whole point: identifiers survived the handover.
        [ "$got" = "$want" ] && [ "$got" -gt 0 ] \
            && report "every identifier resolved ($got/$want)" ok \
            || report "every identifier resolved ($got/$want)" no
        # Two clients were started and nothing opened or closed since, so the
        # count must stay put.  Growth means restored windows were managed a
        # second time and inserted twice; shrinkage means the restore is losing
        # them one handover at a time.
        [ "$want" -eq 2 ] \
            && report "the window count held at 2 ($want)" ok \
            || report "the window count held at 2 (got $want)" no
    done
fi

# Without this the test would pass on a window manager that read the file and
# then died before doing anything with it.  The note comes from runStartupHook,
# which runs immediately after manage_finish on the first sequence, so one per
# window manager means each got a manage sequence all the way through -- and
# the restore happens earlier in that same sequence.
#
# Deliberately not asserted: "river configured a window after the handover".
# The obvious check, a nonzero "sent N tracked configure(s)", is nonzero only
# when a window's dimensions actually change, and a correct restore proposes
# exactly the dimensions the predecessor already had.  So the successful case
# reports zero, and the assertion would be backwards -- it fails when the
# restore works and passes when the layout comes out wrong.
starts=$(grep -c "startup hook skipped" "$WMLOG")
[ "$starts" -eq 3 ] \
    && report "all three window managers completed a manage sequence ($starts)" ok \
    || report "all three window managers completed a manage sequence (got $starts)" no

# The window manager must be the only process running its own path.  Anything
# else with that path in its argv is a shell that was left holding it.
leaked=$(grep -F -- "$WM" "$PSLOG" 2>/dev/null | grep -cF -- "sh -c")
[ "${leaked:-0}" -eq 0 ] \
    && report "no shell left behind by the restarts" ok \
    || report "no shell left behind by the restarts (found ${leaked})" no

backend_trouble=$(grep -nE '^xmonad-river:|protocol error|invalid object' "$WMLOG" 2>/dev/null \
                  | grep -v '^[0-9]*:xmonad-river: note:')
if [ -n "$backend_trouble" ]; then
    report "the backend reported no errors" no
    echo "--- backend errors ---" >&2
    echo "$backend_trouble" | head -20 >&2
else
    report "the backend reported no errors" ok
fi

if [ "$status" -ne 0 ] && [ -s "$WMLOG" ]; then
    echo >&2
    echo "window manager output:" >&2
    sed 's/^/  | /' "$WMLOG" | head -30 >&2
fi
exit "$status"
