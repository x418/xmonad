# Installing the river backend

`INSTALL.md` is upstream's X11 document and covers only `.desktop` files for X
sessions. This is the Wayland counterpart.

Everything below is a failure that cost real debugging time in the prototype
this repo was rewritten from, plus what fixes it. None of it is discoverable
from the code, and most of it fails *silently* — which is why it is written
down rather than left to be rediscovered.

**None of this has been run in a real session.** What this repo has been run
against is a *headless* river — no display, no seat, no GPU — under
`tests/headless-river.sh`, `tests/headless-prompt.sh` and
`tests/headless-restart.sh`, and none of those needs a session file, a display
manager or systemd. So the recipes here are carried over from the prototype,
which did run in a real session. Treat them as informed rather than verified.

## Build and install

```
stack build
stack install                        # puts `xmonad` on PATH
```

The binary must be **installed**, not merely built. Under X11 this is
forgiving: xmonad's recompile machinery runs `~/.xmonad/build` and re-execs
itself, so a missing binary tends to fix itself. river has no equivalent — its
init script just `exec`s a path, and if that path does not exist the window
manager simply never starts.

## river's init script

river runs one command at startup and hands it the window management protocol:

```sh
#!/bin/sh
# ~/.config/river/init  (must be executable)

wm=$HOME/.local/bin/xmonad

if [ ! -x "$wm" ]; then
    # Without this the session is a blank screen with a cursor and nothing in
    # any log: river runs happily with no window management client, so a
    # missing binary is invisible from the seat.  Falling back to a terminal
    # at least leaves something to debug from.
    foot -e sh -c "echo 'xmonad not installed at $wm'; exec sh" &
    exit 0
fi

exec "$wm"
```

Start it with `river -c ~/.config/river/init`.

### Symptom: blank screen, cursor only, nothing in any log

The binary named by the init script is not installed or not executable. See
above — this is the failure the `-x` check exists to prevent.

## Session file

For a display manager, put a session entry in `/usr/share/wayland-sessions/`
(**not** `xsessions/`, which is X11):

```ini
[Desktop Entry]
Name=xmonad (river)
Comment=Tiling window manager, river backend
Exec=/usr/local/bin/xmonad-river-session
Type=Application
DesktopNames=river
```

### Symptom: session exits instantly, empty log

If the session script sources `~/.profile` under `set -u`, and `~/.profile`
reads `HIDPI`, `DISPLAY` or `XDG_DATA_DIRS` unguarded, it dies *before*
reaching any logging. GDM then bounces straight back to the login screen with
nothing written anywhere.

Harmless under the plain `/bin/sh` that GDM's `Xsession` uses; fatal under
`-u`. Wrap the sourcing:

```sh
set +u
. "$HOME/.profile"
set -u
```

### Symptom: half the startup applications silently missing

GDM sources `~/.profile` for X11 sessions (via `/etc/gdm3/Xsession`) but **not**
for Wayland ones — it execs the desktop entry directly. The session inherits
systemd's bare `PATH`, so `~/.local/bin`, `~/.cargo/bin` and anything else
`~/.profile` would have added are absent, and every startup-hook `spawn` of a
program living there fails quietly.

Set `PATH` explicitly in the session script rather than relying on `~/.profile`
being read.

## systemd integration

`graphical-session.target` sets `RefuseManualStart`, so
`systemctl --user start graphical-session.target` fails. It may only be pulled
in by a dependency. Define a target that binds to it:

```ini
# ~/.config/systemd/user/river-session.target
[Unit]
Description=river session
BindsTo=graphical-session.target
Before=graphical-session.target
```

and `systemctl --user start river-session.target` from the session script.

## Things that will not work

- **`xmessage`** and anything else that is an X11 client. Diagnostics go to
  stderr, and therefore to the journal.
- **`gnomeRegister`** — XSMP is X11-only. Use
  `systemctl --user start graphical-session.target` via the target above.
- **Anything drawing its own windows through Xlib directly.** Prompts,
  decorations and tab bars are *not* in this category any more: they draw
  in-process with cairo and pango into a `wl_shm` buffer, and contrib's
  existing drawing code reaches that through `XMonad.Util.River.Compat`, which
  reproduces Xlib's drawing *model* rather than shelling out to `fuzzel` as the
  prototype did. What still fails is a module that calls Xlib itself — a `GC`,
  a `FontStruct`, an `openDisplay`. See `SURVEY.md` in
  `../xmonad-river-contrib` for which ones, and note that a module that does
  not compile is commented out of that package's cabal file, so it is
  unavailable rather than merely broken.
- **Layer surfaces, if the compositor does not offer `river_layer_shell_v1`.**
  This backend binds it when present; without it river closes every layer
  surface on sight, and the symptom is silent — prompts, notifications,
  wallpaper and bars simply never appear, with one `info(wm)` line in river's
  log as the only evidence anywhere.

## Restarting

`M-q` works. river hot-swaps window managers without restarting itself or any
client, so the recompile-and-restart loop survives. `xmonad --restart` from a
terminal does the same thing.

The window set survives with it. river object ids are per-connection and
recycled, so an id written by one window manager means nothing to its
successor — but `river_window_v1.identifier` belongs to the window rather than
to the connection, so the state file is keyed on that instead, and windows come
back on the workspaces you put them on. See the closing section of
`README.river.md`.

`xmonad --restart` reaches the running window manager over a unix socket in
`$XDG_RUNTIME_DIR`, so it works from any terminal in the session and reports
what happened. Every way it can fail — no window manager listening, a refusal,
no reply — is printed and exits non-zero.

### Symptom: `--restart` says there is no running window manager

Either there is none, or `XDG_RUNTIME_DIR` and `WAYLAND_DISPLAY` in that
terminal do not name the session the window manager is running in. The socket
path is derived from both.
