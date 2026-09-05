# Migrating an X11 xmonad config to the river backend

This is the document for someone who has a working `xmonad.hs` under X11 and
wants it to run under Wayland via [river](https://github.com/riverwm/river).

**The organising rule.** *If something cannot be faithfully ported, it is not
exported.* Everything this backend cannot do is absent, so your config fails to
compile at the line that is genuinely unportable, rather than compiling and then
quietly not working.

---

## 1. What has to change outside the config

### The compositor and the session

Nothing about xmonad starts river. river starts *this*, as its window
management client, and it does so by `exec`ing a path from its init script.
`INSTALL.river.md` is the whole story — read it, because most of its failure
modes are silent:

- the binary must be **installed**, not merely built. There is no equivalent of
  X11's "recompile and re-exec fixes it"; river execs a path and a missing path
  is a blank screen with a cursor and nothing in any log.
- the session entry goes in `/usr/share/wayland-sessions/`, **not**
  `xsessions/`.
- GDM does not source `~/.profile` for Wayland sessions, so your startup hook's
  `spawn`s of things in `~/.local/bin` will fail quietly unless the session
  script sets `PATH` itself.

### The build plan

The package names are unchanged, so nothing in your config's `.cabal` or
`package.yaml` has to be renamed: `build-depends: xmonad, xmonad-contrib` still
says what you mean. What changes is which sources those names resolve to.
Point your build at this fork and, if you use contrib, at its river
counterpart:

```yaml
# stack.yaml
resolver: lts-24.53
packages:
- .
- ../xmonad-river/
- ../xmonad-river-contrib/
```

The X11 `xmonad` and `xmonad-contrib` checkouts must *not* also be in
`packages`: they are unbuildable without libX11 headers, and the river
checkouts stand in under the same package names.

Two build-time requirements this backend has and the X11 one does not:

- **`libxkbcommon`** (a `pkgconfig-depends`). It turns the compositor's keymap
  and a raw keycode into a keysym and the text it produces — needed for prompts,
  where a binding per keysym could express a shortcut but not typing.
- **the threaded runtime**, already set on the `xmonad` executable here and
  passed by `xmonad --recompile`. If you build your own executable rather than
  using either, it needs `-threaded`: the event loop and the worker are two
  threads, `sendRestart` interrupts the loop with an async exception, a prompt
  runs on a thread of its own, and the calls into libxkbcommon are safe FFI.

### The programs your config spawns

Your config's `spawn` calls are strings, so they compile no matter what. They
are also the part of a migration that fails most visibly at runtime. Anything
that is an X11 client will not run:

| what it did | why it stops | what replaces it |
| --- | --- | --- |
| `xmessage` | X11 client | it is gone from the API too; diagnostics go to stderr, and so to the journal |
| `xmobar`, `dzen2`, `trayer` | X11 clients | a layer-shell bar (waybar and friends). This backend binds `river_layer_shell_v1`, and a bar's exclusive zone shrinks the tiling area for you |
| `xrandr`, `xset`, `xsetroot` | X server configuration | river's own configuration, or the relevant Wayland protocol |
| `scrot`, `xdotool`, `xclip` | X server access | `grim`/`slurp`, `wtype`, `wl-clipboard` |
| `slock`, `i3lock` | X11 grabs | an `ext-session-lock-v1` locker |

The defaults in `XMonad.Config` moved for the same reason: `terminal` is now
`foot` and `mod-p` launches `fuzzel`. If your config sets `terminal` itself,
nothing here affects you.

One runtime dependency is load-bearing rather than cosmetic:
`XMonad.Util.XSelection` shells out to **`wl-paste`** (from `wl-clipboard`),
because Wayland offers the selection only to the client holding keyboard focus
and a window manager is not in the focus chain at all. It warns once on stderr
if the program is missing. It is not a build dependency and cannot be checked
for at compile time.

---

## 2. What has to change inside the config

### Imports

`XMonad` no longer re-exports `Graphics.X11` or `Graphics.X11.Xlib.Extras`.
Under X11 those two supply 1458 names — keysyms, masks, geometry types, and
several hundred Xlib calls. Reproducing that surface over Wayland would mean
hundreds of functions that typecheck and do nothing.

The names from it that port faithfully are exported from `XMonad.Core`
(and so from `XMonad`) instead, and you do not have to import anything new to
get them:

```
Window, Rectangle(..), Position, Dimension, KeyMask, KeySym, Button, ButtonMask, Pixel
shiftMask, lockMask, controlMask, mod1Mask .. mod5Mask, noModMask
button1 .. button5
xK_*  (450 of them, generated from the X11 package's own values)
noSymbol, stringToKeysym, keysymToString
SizeHints(..), WindowAttributes(..), withWindowAttributes, getWindowAttributes,
getGeometry, getWMNormalHints
setWindowBorder, setWindowBorderWidth
EventType, keyPress, keyRelease
```

So an explicit `import Graphics.X11` or `import Graphics.X11.Xlib.Extras` in
your config is deleted, not replaced. If you were relying on the re-export
without importing anything, most of it keeps working.

Anything river-specific lives in `XMonad.River`, which you import explicitly.
Importing it is your config saying, out loud, that it is a river config.

### `XConfig` fields that are gone

```haskell
clientMask, rootMask   -- there are no event masks to select
```

Delete them. If you were setting them to add `pointerMotionMask` or similar,
what you actually wanted is discussed under *the event hook*, below.

`XConf` loses `theRoot`, for the obvious reason. `display` survives, and so
does `withDisplay`: `type Display = Connection`, the handle to the Wayland
server, which the protocol itself calls `wl_display`. What does not survive is
anything that called an Xlib function on it — and those names are absent, so
the failure lands on the unportable call rather than on `withDisplay`.

### Border colours must be hex

```haskell
normalBorderColor = "gray"        -- X11: resolved against the server colormap
normalBorderColor = "#dddddd"     -- river: RGBA channel values, no colormap
```

There is no colormap to resolve a name against. `initColor` is therefore gone;
`XMonad.River.parseColor` / `parseColorMaybe` answer the same question — *is
this a usable colour* — without one, and `parseColor` gives opaque black rather
than failing. `setWindowBorderWithFallback` is still there and still takes a
`Pixel`.

### `mod-shift-q` means something different

Under X11 the default quit binding exited the process and the server noticed.
Under river, exiting only hands the seat to the next window manager: the
compositor keeps running and so does every client. The default is now
`XMonad.River.exitSession`, which ends the Wayland session. If your config
rebinds quit to `io exitSuccess`, change it to `exitSession` or you will exit
into a session with no window management.

`mod-q` — recompile and restart — is unchanged, and works. river hot-swaps
window managers without restarting itself or any client.

### The event hook

`handleEventHook :: Event -> X All` keeps its type, but `Event` is a different
type. It is no longer an Xlib event union; river delivers window management as
a protocol with a manage/render sequence rather than a stream of events to
interpret. The constructors are:

```haskell
DestroyWindowEvent{ev_window}   WindowAdded{ev_window}
WindowTitleChanged{ev_window, ev_text}
WindowAppIdChanged{ev_window, ev_text}
OutputAdded/OutputRemoved{ev_output}   SeatAdded/SeatRemoved{ev_seat}
ScreenLayoutChanged
SessionLocked   SessionUnlocked
KeyPressed{ev_state, ev_keysym}
TimerFired !Int
```

Two of these are worth knowing about:

- **`ScreenLayoutChanged`** is what `XMonad.Hooks.Rescreen` now watches. Under
  X11 a config had to work this out itself, from `ConfigureNotify` on the root
  window versus `RRScreenChangeNotify`, and de-duplicate the burst Xorg emitted.
  Here it is sent only when the screen rectangles genuinely differ.
- **`KeyPressed`** does *not* arrive for ordinary keybindings — those run their
  action directly. It reaches a config only while something (a prompt, a submap)
  is holding keys captured, because the window manager only learns about keys it
  has created a binding for.

A hook that pattern-matched `ClientMessageEvent`, `MotionEvent` or
`ButtonEvent` has no equivalent and will not compile. That is the intended
outcome.

### Manage-hook queries

`title`, `className` and `appName` all work, from `river_window_v1.title` and
`app_id`. **`className` and `appName` return the same string**: river has no
separate instance name, so a config distinguishing the two — the classic
`className =? "Firefox" <&&> appName =? "Navigator"` — needs rewriting.

`stringProperty` and `getStringProperty` are gone, along with `getAtom` and the
`atom_*` names: Wayland has no window properties and no atom namespace at all.
The nearest things a window can be asked are `app_id`, `title`,
`unreliable_pid` and `identifier`.

`XMonad.River.windowParent` is what `WM_TRANSIENT_FOR` answered, under the name
Wayland gives it.

---

## 3. Names that are gone, and what to reach for instead

Every absence is justified by name in
[`tests/api/unportable.txt`](tests/api/unportable.txt), which is enforced: a
name dropped without an entry fails the API test, and so does a stale entry for
a name river has since grown. In summary:

| gone | why | what to do |
| --- | --- | --- |
| `getAtom`, `atom_WM_STATE`, `atom_WM_PROTOCOLS`, `atom_WM_DELETE_WINDOW`, `atom_WM_TAKE_FOCUS` | no atom namespace | the *purposes* survive: `hide`/`show` for `WM_STATE`, `river_window_v1.close` for delete, `river_seat_v1.focus_window` for take-focus — all reached through ordinary xmonad operations |
| `stringProperty`, `getStringProperty`, `setWMState` | no window properties | as above |
| `initColor` | no colormap | `XMonad.River.parseColorMaybe` |
| `mkGrabs` | returns `KeyCode`s, which need the keymap | bindings are river objects; `XMonad.River.grabKeys` / `ungrabKeys` |
| `unGrab` | `pure ()` would be true about grabs and misleading about handing the keyboard to a locker | a locker takes input via `ext-session-lock-v1`; nothing to call |
| `setButtonGrab` | river decided this when the pointer binding was created | nothing to call |
| `clearEvents` | correct as `pure ()`, but its `EventMask` argument cannot be given a meaning | delete the call |
| `sendReplace` / `--replace` | river permits one window manager and answers a second with `unavailable` | `--restart` is the swap, and clients survive it |
| `xmessage` | X11 client | `trace`, or stderr; it lands in the journal |
| `manage`, `tileWindow`, `setTopFocus`, `setFocusX`, `setInitialProperties` | implementable, but river only permits window management state to change during a manage sequence, and every caller assumes "now" | held back deliberately; see below |

That last row is the one to understand, because it is a *timing* decision
rather than a capability one. See §4.

Things people expect to be gone and are not: `withDisplay`, `withWindowAttributes`,
`getGeometry`, `getWMNormalHints`, `setWindowBorder`, `setWindowBorderWidth`,
`cleanMask`, `extraModifiers`, `isRoot`, `cacheNumlockMask`, `StateFile`,
`writeStateToFile`, `readStateFile`, `restart`, `warpPointer`, `mouseDrag`,
`mouseMoveWindow`, `mouseResizeWindow`, `float`, `floatLocation`, the whole size
hints family.

---

## 4. Runtime model differences

None of these break compilation. They are the ones to keep in mind when
something behaves oddly.

**Window management state changes only during a manage sequence.** X11 let a
window manager act at any moment. river has a manage/render cycle, and window
management state (dimensions, focus, adoption) may only change inside it, while
rendering state (position, order, borders, hide/show) is applied at
`render_finish`. Ordinary keybinding actions are already inside a sequence and
you will not notice. Code running on a **timer or a forked thread** is not:

```haskell
import XMonad.River (postAction, manageDirty)

-- from any thread, with the XConf you captured:
postAction conf $ do
    ...            -- queued; runs at the start of the next manage sequence
    manageDirty    -- ask river for a sequence, because it cannot see what changed
```

The event loop owns the connection outright, so a helper thread cannot touch it;
`postAction` is the only way in.

**Geometry is what the layout decided, not what a server was asked.** river
never reports where a window is, because the window manager is the thing that
chose. `XMonad.River.windowRect` answers from the last layout run, and returns
`Nothing` for a window that is not currently placed — one on an off-screen
workspace, or one that has just appeared. Under X11 those still had geometry, so
code that assumed an answer needs one supplied.

**`moveResizeWindow` is two operations here.** Position is rendering state and
may be set any time; dimensions are window management state and may only be
proposed during a manage sequence. Calling it outside one silently drops the
resize.

**Raising is a standing request, not a one-off.** X11's restack stuck until
something changed it; river's render sequence re-applies the layout's order on
every frame. `raiseWindow` and `restackWindows` record a request rather than
performing an act.

**Layer shell is what makes bars, wallpaper and notifications appear at all.**
This backend binds `river_layer_shell_v1` when the compositor offers it, and
prefers each output's *usable* area — what is left once exclusive zones are
claimed — so a bar is not tiled over. On a compositor without it, river closes
every layer surface on sight and the symptom is silent: one `info(wm)` line in
river's log and no prompts, no bar.

---

## 5. xmonad-contrib

Use the `xmonad-on-river` fork of `xmonad-contrib`. Its `SURVEY.md` is the
measurement, kept up to date by `tests/survey.sh`:

| | count |
| --- | --- |
| compiled | **283** |
| failed | 25 |
| skipped behind a failed import | 26 |
| total | 334 |

**What does not**, grouped by cause:

- *EWMH*: `XMonad.Hooks.EwmhDesktops` fails, and takes 11 modules with it,
  including `XMonad.Config.Desktop`, `XMonad.Layout.Fullscreen` and every
  desktop-environment config module. If your config imports
  `XMonad.Config.Desktop`, this is the single biggest thing to plan around.
- *window properties*: `XPropManage`, `StringProp`, `TaffybarPagerHints`,
  `DebugWindow`.
- *raw X events*: `UpdateFocus`, `Minimize`, `ServerMode`, `MouseResize`
  (and via it `SimpleFloat`, `DecorationMadness`).
- *Xlib drawing/display*: `TreeSelect`, `Replace`, `DebugKeyEvents`,
  `NoTaskbar`, `DynamicBars`.
- *the root window*: `EasyMotion`, `XMonad.Util.Paste` (and via it `KeyRemap`,
  `Prefix`).
- *compositing, via `getAtom`*: `FadeInactive`, and `FadeWindows` behind it.
- *misc*: `XMonad.Util.Ungrab`, `XMonad.Hooks.ScreenCorners`,
  `XMonad.Layout.BorderResize`, `XMonad.Layout.MouseResizableTile`.

This cost is deliberate and falls where it was always going to fall: a contrib
module that touches `withDisplay` to call Xlib, or size hints, or window
properties, does not compile here.
