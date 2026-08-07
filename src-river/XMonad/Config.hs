{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- The river backend's copy of @src\/XMonad\/Config.hs@.
--
-- Differences from the X11 default, and only these:
--
-- * There are no event masks to select, so @clientMask@ and @rootMask@ are
--   gone from 'XConfig' and from here.
-- * The default terminal and launcher follow the Wayland convention.
-- * @mod-q@ restarts through 'sendRestart' rather than shelling out to
--   @xmonad --restart@, and the help binding writes to stderr instead of
--   spawning an @xmessage@, which is an X11 client.
-- * @mod-shift-q@ ends the Wayland session rather than exiting the process,
--   because under river exiting only hands the seat to the next window
--   manager -- the compositor keeps running.
--
------------------------------------------------------------------------------

module XMonad.Config (defaultConfig, Default(..)) where

--
-- Useful imports
--
import XMonad.Core as XMonad hiding
    (workspaces,manageHook,keys,logHook,startupHook,borderWidth,mouseBindings
    ,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor,focusFollowsMouse
    ,handleEventHook,clickJustFocuses)
import qualified XMonad.Core as XMonad
    (workspaces,manageHook,keys,logHook,startupHook,borderWidth,mouseBindings
    ,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor,focusFollowsMouse
    ,handleEventHook,clickJustFocuses)

import XMonad.Layout
import XMonad.Operations
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import Data.Default.Class
import Data.Monoid
import qualified Data.Map as M

-- | The default number of workspaces (virtual screens) and their names.
workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int]

-- | modMask lets you specify which modkey you want to use.
--
-- These are numerically X11's masks, because @river_seat_v1.modifiers@ uses
-- X11's values: shift=1, ctrl=4, mod1=8, mod3=32, mod4=64, mod5=128.
defaultModMask :: KeyMask
defaultModMask = mod1Mask

-- | Width of the window border in pixels.
borderWidth :: Dimension
borderWidth = 1

-- | Border colors for unfocused and focused windows, respectively.
--
-- Unlike X11 these must be hex: river takes RGBA channel values directly and
-- there is no colormap to resolve a name like @\"gray\"@ against.
normalBorderColor, focusedBorderColor :: String
normalBorderColor  = "#dddddd"
focusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Window rules

manageHook :: ManageHook
manageHook = idHook

------------------------------------------------------------------------
-- Logging

logHook :: X ()
logHook = return ()

------------------------------------------------------------------------
-- Event handling

-- | Defines a custom handler function for river events. The function should
-- return (All True) if the default handler is to be run afterwards.
handleEventHook :: Event -> X All
handleEventHook _ = return (All True)

-- | Perform an arbitrary action at xmonad startup.
startupHook :: X ()
startupHook = return ()

------------------------------------------------------------------------
-- Extensible layouts

-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice.
layout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Key bindings:

-- | The preferred terminal program.
terminal :: String
terminal = "foot"

-- | Whether focus follows the mouse pointer.
focusFollowsMouse :: Bool
focusFollowsMouse = True

-- | Whether a mouse click select the focus or is just passed to the window
clickJustFocuses :: Bool
clickJustFocuses = True

-- | The xmonad key bindings. Add, modify or remove key bindings here.
keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask,               xK_p     ), spawn "fuzzel") -- %! Launch fuzzel
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), exitSession) -- %! Quit xmonad, ending the Wayland session
    , ((modMask              , xK_q     ), io sendRestart) -- %! Restart xmonad

    , ((modMask .|. shiftMask, xK_slash ), helpCommand) -- %! Print a summary of the default keybindings
    -- repeat the binding for non-American layout keyboards
    , ((modMask              , xK_question), helpCommand)
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
  where
    -- xmonad pops an xmessage here.  That is an X11 client, so the summary
    -- goes to stderr, which under a Wayland session lands in the journal.
    helpCommand :: X ()
    helpCommand = trace help

-- | Mouse bindings: default actions bound to mouse events.
--
-- Interactive move and resize run over river's seat operation cycle
-- (@op_start_pointer@ \/ @op_delta@ \/ @op_release@) rather than by the window
-- manager grabbing the pointer, but the bindings are the ones xmonad has
-- always had.
mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), \w -> focus w >> mouseMoveWindow w
                                         >> windows W.shiftMaster)
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), windows . (W.shiftMaster .) . W.focusWindow)
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), \w -> focus w >> mouseResizeWindow w
                                         >> windows W.shiftMaster)
    ]

instance (a ~ Choose Tall (Choose (Mirror Tall) Full)) => Default (XConfig a) where
  def = XConfig
    { XMonad.borderWidth        = borderWidth
    , XMonad.workspaces         = workspaces
    , XMonad.layoutHook         = layout
    , XMonad.terminal           = terminal
    , XMonad.normalBorderColor  = normalBorderColor
    , XMonad.focusedBorderColor = focusedBorderColor
    , XMonad.modMask            = defaultModMask
    , XMonad.keys               = keys
    , XMonad.logHook            = logHook
    , XMonad.startupHook        = startupHook
    , XMonad.mouseBindings      = mouseBindings
    , XMonad.manageHook         = manageHook
    , XMonad.handleEventHook    = handleEventHook
    , XMonad.focusFollowsMouse  = focusFollowsMouse
    , XMonad.clickJustFocuses   = clickJustFocuses
    , XMonad.handleExtraArgs = \ xs theConf -> case xs of
                [] -> return theConf
                _ -> fail ("unrecognized flags:" ++ show xs)
    , XMonad.extensibleConf     = M.empty
    }

-- | The default set of configuration values itself
{-# DEPRECATED defaultConfig "Use def (from Data.Default, and re-exported by XMonad and XMonad.Config) instead." #-}
defaultConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
defaultConfig = def

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch terminal",
    "mod-p            Launch fuzzel",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "mod-Shift-/      Show this help message with the default keybindings",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit, ending the Wayland session",
    "mod-q        Restart xmonad",
    "",
    "-- Workspaces & screens",
    "mod-[1..9]         Switch to workSpace N",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
