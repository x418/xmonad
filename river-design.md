# river backend for xmonad, behind a cabal flag

A plan for building the river/Wayland backend *inside* the xmonad fork, selected
by a manual cabal flag, rather than as the separate `xmonad-river` package that
`~/env/xmonad-river` prototypes today.

## Goal

One source tree, two builds:

```
cabal build                      # X11 backend, byte-for-byte upstream behaviour
cabal build -f river             # river/Wayland backend
```

Both produce a library named `xmonad` exporting the same API, so that
`xmonad-contrib` and an unmodified `xmonad.hs` compile against either.

That last clause is the whole design constraint. Everything below follows from
it.

## What the prototype already settled

`~/env/xmonad-river` is a working answer to "can this be done at all", and its
conclusions carry over unchanged:

- **river defers all window management policy** to a separate process speaking
  `river-window-management-v1`. Custom layouts, `M-q` restart, and the
  manage-hook ordering guarantee all survive. This is not true of sway.
- **Keysyms and modifier masks are numerically identical** between X11 and
  xkbcommon/river. `mod4Mask` is 64 in both. Key descriptions need no
  translation.
- **The wire protocol can be spoken directly**, no `libwayland`, because none of
  the river WM protocols pass file descriptors. `store-core` matches Wayland's
  host-endian 32-bit-aligned format natively.
- **Protocol bindings are generated and checked in**, not generated at build
  time, to keep the recompile loop fast.

What changes here is only *packaging*: the prototype vendors copies of
`XMonad.Core`, `XMonad.Operations`, `XMonad.Layout`, `XMonad.ManageHook` and
`XMonad.StackSet` alongside its river modules. Two copies of those modules will
drift. Folding them back into one tree is the point of this exercise, and the
API test in §3 is what makes the drift visible when it happens.

## The size of the problem

Export counts from `ghc --show-iface` on the current X11 build:

| Module | Exports | Of which re-exported from `X11` |
| --- | --- | --- |
| `XMonad` | 1501 | 1335 |
| `XMonad.Operations` | 62 | 0 |
| `XMonad.Core` | 58 | 0 |
| `XMonad.StackSet` | 46 | 0 |
| `XMonad.ManageHook` | 19 | 0 |
| `XMonad.Layout` | 16 | 0 |
| `XMonad.Main` | 3 | 0 |
| `XMonad.Config` | 2 | 0 |

Two things to read off this:

**`XMonad` re-exports 1335 names from the `X11` package.** `XMonad.hs` does
`import Graphics.X11` and `import Graphics.X11.Xlib.Extras` and re-exports both
wholesale. Under `-f river` every one of those names must still exist, or
configs and contrib break on names that have nothing to do with window
management (`xK_*`, `*Mask`, `button*`, geometry types). This is the bulk of the
compat surface, and it is also the easy part — most are constants.

**Only 206 names are xmonad's own**, and the prototype currently covers roughly
half of them: `XMonad.Core` 44 of 58, `XMonad.Operations` 17 of 62. Names the
prototype does not yet export include `manage`, `unmanage`, `focus`, `hide`,
`reveal`, `rescreen`, `floatLocation`, `windowBracket`, `tileWindow`,
`applySizeHints`, `mouseDrag`, `setWMState`, `withDisplay`, `recompile`,
`spawnPID`, `xmessage`. Closing that gap is the real work of §2, and much of it
is shims rather than implementations.

---

## 1. The cabal flag

```cabal
flag river
  description: Build against river/Wayland instead of X11.
  default:     False
  manual:      True
```

`manual: True` matters: without it cabal's solver is free to flip the flag to
resolve a build plan, and "my X11 build silently became a Wayland build because
libX11 was missing" is a bad afternoon.

In the library stanza:

```cabal
library
  exposed-modules: XMonad
                   XMonad.Config
                   XMonad.Core
                   XMonad.Layout
                   XMonad.Main
                   XMonad.ManageHook
                   XMonad.Operations
                   XMonad.StackSet
  build-depends:   base, containers, data-default-class, directory, filepath
                 , mtl, process, setlocale, time, transformers, unix

  if flag(river)
    cpp-options:     -DRIVER
    hs-source-dirs:  src src-river
    exposed-modules: XMonad.Backend
    other-modules:   XMonad.Backend.River
                     XMonad.River.Wire
                     XMonad.River.Connection
                     XMonad.River.WM
                     XMonad.River.Protocol.WindowManagement
                     XMonad.River.Protocol.XkbBindings
    build-depends:   bytestring, network, store-core
  else
    hs-source-dirs:  src src-x11
    exposed-modules: XMonad.Backend
    other-modules:   XMonad.Backend.X11
    build-depends:   X11 >= 1.10 && < 1.11
```

Notes on the mechanics:

- **Separate `hs-source-dirs` per branch, not one directory with CPP'd module
  bodies.** Each branch gets its own `src-x11/XMonad/Backend.hs` and
  `src-river/XMonad/Backend.hs`. This is the main reason §2 stays small: the
  file-level switch is the cabal file, not `#ifdef`.
- **A flag cannot rename a component**, only enable or disable one — the
  prototype's README documents hitting this with executables. It is not a
  problem here because both builds want the same library name; it *is* a problem
  if you later want `xmonad` and `xmonad-river` installed side by side, which
  would need two `executable` stanzas each `buildable: False` under the opposite
  flag.
- **`-Wunused-packages` is on for GHC > 8.8.4.** Deps in the inactive branch are
  not in scope, so this stays clean — but any dependency that only one backend
  uses must move inside the conditional, or the other build warns (and `-Werror`
  under `-f pedantic` turns that into a failure).
- The `properties` test-suite currently `build-depends: X11`. It uses X11 only
  for `Rectangle`; under `-f river` that dep must come from the compat module
  instead. Same conditional treatment.

## 2. CPP strategy

### The rule: CPP switches *modules*, not *expressions*

Upstream `XMonad.Operations` is 899 lines; the prototype's river equivalent is
211. `XMonad.Core` is 898 against 483. These are not two implementations of the
same code with a few differing lines — they are different programs that agree on
an interface. Interleaving them with `#ifdef` would produce a file neither
backend's contributor can read, and which no single compile ever checks in full.

So CPP appears in exactly one shape, once per file that needs it:

```haskell
-- src/XMonad/Backend.hs is NOT this file; this is the pattern used by
-- src-x11/XMonad/Backend.hs and src-river/XMonad/Backend.hs, which each
-- re-export their own implementation under one name.
module XMonad.Backend (module B) where

#ifdef RIVER
import XMonad.Backend.River as B
#else
import XMonad.Backend.X11 as B
#endif
```

Everything above `XMonad.Backend` — `XMonad.Operations`, `XMonad.ManageHook`,
`XMonad.Layout`, most of `XMonad.Core` — imports `XMonad.Backend` and contains
no CPP at all.

Budget: **under 20 `#ifdef`s in the whole tree.** If it grows past that, the
abstraction boundary is in the wrong place and the fix is to move a function
behind `XMonad.Backend`, not to add another conditional. Worth stating as a
reviewable rule, because CPP creep is exactly how a two-backend tree rots.

### The compat layer is the mechanism, not a convenience

The prototype's `XMonad/River/X11Compat.hs` (415 lines) redefines `Window`,
`Rectangle`, `Position`, `Dimension`, `KeyMask`, `KeySym`, `Button`, the
modifier masks, the button constants and the `xK_*` keysyms against river. It
reads today like a pragmatic shim. Under this design it is load-bearing: **it is
what makes requirement 3 satisfiable at all.**

The reasoning: `withDisplay :: (Display -> X a) -> X a` is exported by
`XMonad.Core` and used throughout contrib. If `Display` simply does not exist
under `-f river`, the API differs and contrib does not compile. If instead the
river backend *defines* `Display` as its `Connection`, the name and the
signature survive, and only the semantics change. The prototype already made
this choice for `Window` (`type Window = ObjectId`) and `Rectangle`; the plan is
to extend it to the rest of the X11-shaped surface:

| Type | X11 | river |
| --- | --- | --- |
| `Window` | `XID` | `ObjectId` (done) |
| `Rectangle` | X11 struct | compat struct (done) |
| `KeySym`, `KeyMask`, `Button` | X11 | identical numerics (done) |
| `Display` | `Display` | `Connection` |
| `Pixel` | `Word64` | packed RGBA `Word32` |
| `Event` | `Extras.Event` | `RiverEvent` |
| `EventMask` | `Word64` | `Word64`, ignored |
| `Atom` | `Word64` | `Word64`, ignored |

`Pixel` deserves a note: the prototype's `XConf` stores borders as
`(Word32, Word32, Word32, Word32)`, which is honest about river's RGBA but
changes the type of the exported `normalBorder`/`focusedBorder` fields. Packing
into a single `Pixel`-shaped `Word32` keeps the API identical and pushes the
unpacking into the render path, which is where it belongs anyway.

### Three tiers of divergence, and how each is handled

1. **Same name, same type, same behaviour** — `StackSet` entirely, the
   `Query`/`ManageHook` algebra, layout arithmetic. Shared code, no CPP.
2. **Same name, same type, different implementation** — `manage`, `unmanage`,
   `focus`, `hide`, `reveal`, `rescreen`, `tileWindow`. Behind `XMonad.Backend`.
3. **Same name, same type, no meaningful behaviour** — `stringProperty`,
   `isFullscreen`, `setEwmhActivateHook`, `mouseDrag`, `gnomeRegister`. These
   must still exist and typecheck. Keep the prototype's `warnUnimplemented`,
   which logs once per process to stderr, and therefore into the journal. Silent
   no-ops are worse: a config rule that never fires looks like a config bug.

Tier 3 is the honest cost of this design, and the README of the prototype
already enumerates most of its membership. The API test cannot distinguish tier
2 from tier 3 — that is what the status table in the README is for, and it
should move into this repo alongside the code.

### `XConf` is the one place inline CPP is unavoidable

`XConf` is a record whose fields genuinely differ:

```haskell
-- X11
data XConf = XConf { display :: Display, theRoot :: !Window, ... }
-- river (prototype)
data XConf = XConf { riverConn :: !Connection, riverManager :: !ObjectId
                   , riverWindows :: !(IORef (M.Map ObjectId RiverWindow)), ... }
```

Options considered:

- **Make `XConf` abstract.** Cleanest, but `display` and `theRoot` are used
  directly by contrib. Rejected: it breaks the X11 API, which is the one thing
  this fork must not do.
- **Inline `#ifdef` inside the `data XConf` declaration.** One contained block,
  ~15 lines. The X11 field set is preserved exactly; river adds its own fields
  and defines `display`/`theRoot` in terms of the compat types above.

Take the second. It costs one `#ifdef` and keeps `withDisplay`, `asks display`
and `theRoot` working in both builds. Note this is the one construct where the
API test earns its keep immediately: it is very easy to add an X11-only field
with an exported selector and not notice.

## 3. The API-equality test

### Mechanism

`ghc --show-iface` on the built `.hi` files emits both an export list and full
type signatures:

```
exports:
  withDisplay
  ...
withDisplay :: (Graphics.X11.Xlib.Types.Display -> X a) -> X a
```

Verified working on the current build (GHC 9.4.7). Two properties matter:

- Locally-defined exports print **unqualified**; re-exports print **fully
  qualified** (`Graphics.X11.Types.mod4Mask`). So the 1335 X11 re-exports appear
  under `Graphics.X11.*` in one build and `XMonad.Backend.Compat.*` in the
  other. **Comparison must normalize the defining module away.**
- Type signatures are fully qualified too, so `Display` renders as
  `Graphics.X11.Xlib.Types.Display` vs whatever river names it. Same
  normalization applies, and it must be applied to signature text, not just to
  export names.

Do not use the `export-list hash` field as a shortcut. It is computed over
qualified names and will differ between backends by construction.

### Shape

A golden file, not a live two-way comparison — the two backends cannot be built
into the same test binary, and requiring libX11 to be present to run the river
test suite defeats the purpose.

```
tests/api/xmonad-api.golden      # checked in, normalized, sorted
tests/api/dump-api.sh            # $1 = builddir; emits normalized surface
```

`dump-api.sh` walks the eight exposed modules, runs `--show-iface`, extracts the
`exports:` section and the top-level signature lines, strips module
qualification down to the unqualified name, sorts, and writes the result.

The test then becomes: **each backend, built and dumped, must reproduce the
golden file byte for byte.** CI runs it twice, once per flag setting. A
mismatch is either a real API break or an intentional change that must be
accompanied by a regenerated golden file in the same commit — which is exactly
the review signal wanted.

Note the golden file also pins the API against *upstream* drift: when a rebase
onto xmonad HEAD adds an export, the golden file fails until the river backend
grows the corresponding name. Given this fork's whole purpose is to track
upstream, that is the more valuable of the two guarantees.

### What normalization must and must not erase

Erasing the defining module is necessary, and it is also the test's main
weakness: `Display` under river being a `Connection` is invisible to a
name-normalized comparison. That is deliberate — the test's job is "does contrib
still compile", not "does it still work". The tier-3 table is the record of
where behaviour diverges, and no static check substitutes for it.

Do **not** normalize away:

- arity or argument order,
- type variables and constraints (a signature that gains `MonadIO m =>` is a
  break),
- class methods and instance heads for exported classes,
- data constructor and field names (this is what catches the `XConf` hazard).

### Open question: does the golden file cover `XMonad`'s 1335 re-exports?

Including them makes the golden file ~1500 lines and turns every X11 package
bump into a diff. Excluding them means nothing checks that the compat module
still covers the keysym surface, which is the most mechanically breakable part.

Recommendation: include them, in a separate `xmonad-reexports.golden`, so the
noisy file and the meaningful 206-name file can be reviewed independently. Split
the check accordingly, and pin the `X11` dependency tightly (it already is:
`>= 1.10 && < 1.11`) so the noisy file only moves deliberately.

## Staging

1. Land the flag with an empty river branch: `-f river` fails to build, but the
   cabal plumbing, `hs-source-dirs` split and CI matrix entry are in place.
2. Add `dump-api.sh` and generate the golden file from the X11 build alone. It
   is useful before the river backend exists, as an upstream-drift tripwire.
3. Move the prototype's `XMonad.River.*` modules in under `src-river/`,
   unmodified. They already build.
4. Introduce `XMonad.Backend` and make the X11 build route through it, with the
   river branch still absent. This is the largest and least interesting commit;
   it should not change behaviour at all, and the golden file proves it.
5. Delete the prototype's vendored `XMonad.Core`/`Operations`/`Layout`/
   `ManageHook`/`StackSet` copies, wiring the river backend into the shared
   ones. Golden file goes green in both configurations.
6. Close the tier-3 gaps by export count until both goldens match.

Steps 1, 2 and 4 are worth doing even if the river work stalls: they cost
nothing under X11 and make the fork's rebase-onto-upstream loop safer.

## Things this plan does not address

- **Nothing has been run against a live river.** The prototype's status section
  is explicit that all its evidence is compile-time and unit-test only. This
  plan inherits that; the API test does not change it.
- `Main.hs` / `XMonad.Main` argument handling, session management and the
  `--restart` path differ per backend and are not analysed here.
- Extensible state does not survive restart under river (the prototype notes
  `PersistentExtension` degrades to `StateExtension`). That is an API-visible
  behaviour difference the golden file will not catch.
- Whether to upstream any of this. The compat-layer approach is invasive enough
  that it probably lives in the fork indefinitely, which makes step 2's
  drift tripwire more important, not less.
