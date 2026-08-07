{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Core
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- The 'X' monad, a state monad transformer over 'IO', for the window
-- manager state, and support routines.
--
-- This is the river backend's copy of @src\/XMonad\/Core.hs@.  It is
-- deliberately kept as close to that file as it can be -- 84% of it is shared,
-- and the divergence is tracked as @patches\/XMonad\/Core.hs.patch@ so that an
-- upstream change either applies cleanly or conflicts loudly.  See
-- tests/check-copies.sh.
--
-- What differs, and only what differs:
--
-- * 'XConf' holds a river 'Connection' where the X11 build holds an Xlib
--   @Display@, but the field and the type keep their names: @type Display =
--   Connection@, because that is what the value is.  So 'withDisplay' works.
--   What has no Wayland counterpart is everything X11 reached for *through*
--   the display -- @getAtom@, @atom_WM_*@, @withWindowAttributes@ -- and those
--   are absent rather than present and inert.  See tests/api/unportable.txt.
--   'isRoot' is the exception: "is this the root window" has a correct total
--   answer here.
-- * @clientMask@ and @rootMask@ are gone from 'XConfig': river delivers exactly
--   the events the window management protocol defines, with no mask to select.
-- * 'Event' is river's event type rather than Xlib's.
-- * Border colours are RGBA quadruples, which is what @set_borders@ takes,
--   rather than an X11 @Pixel@ resolved against a colormap.
--
-----------------------------------------------------------------------------

module XMonad.Core (
    X(..), WindowSet, WindowSpace, WorkspaceId,
    ScreenId(..), ScreenDetail(..), XState(..),
    XConf(..), XConfig(..), LayoutClass(..),
    Layout(..), readsLayout, Typeable, Message,
    SomeMessage(..), fromMessage, LayoutMessages(..),
    StateExtension(..), ExtensionClass(..), ConfExtension(..),
    runX, catchX, userCode, userCodeDef, io, catchIO, installSignalHandlers, uninstallSignalHandlers,
    withDisplay, withWindowSet, isRoot, runOnWorkspaces,
    spawn, spawnPID, xfork, recompile, trace, whenJust, whenX, ifM,
    getXMonadDir, getXMonadCacheDir, getXMonadDataDir, binFileName,
    ManageHook, Query(..), runQuery, Directories'(..), Directories, getDirectories,
    -- * Types that port unchanged
    --
    -- | The X11 build gets these from @XMonad@'s re-export of "Graphics.X11".
    -- River does not re-export that module, so the handful of names from it
    -- that /do/ port faithfully are exported here instead -- a config still
    -- has to be able to write @LayoutClass l Window@.
    Window, Rectangle(..), Position, Dimension, KeyMask, KeySym, Button,
    ButtonMask,
    shiftMask, lockMask, controlMask, mod1Mask, mod2Mask, mod3Mask, mod4Mask,
    mod5Mask, noModMask,
    button1, button2, button3, button4, button5,
    module XMonad.River.Keysym,
    SizeHints(..),
    Event(..), Connection, Display, sendRestart,
  ) where

import XMonad.StackSet hiding (modify)

import Prelude
import Control.Concurrent (ThreadId, myThreadId)
import Control.Exception (fromException, try, bracket_, throw, finally, SomeException(..))
import qualified Control.Exception as E
import Control.Applicative ((<|>), empty)
import Control.Monad.Fail
import Control.Monad.Fix (fix)
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (filterM, guard, unless, void, when)
import Data.Char (isSpace)
import Data.IORef
import Data.Semigroup
import Data.Traversable (for)
import Data.Time.Clock (UTCTime)
import Data.Default.Class
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isInfixOf, intercalate, (\\))
import System.FilePath
import System.IO
import System.Info
import System.Posix.Env (getEnv)
import System.Posix.Process (executeFile, forkProcess, getAnyProcessStatus, createSession)
import System.Posix.Signals
import System.Posix.IO
import System.Posix.Types (ProcessID)
import System.Process
import System.Directory
import System.Exit
import Data.Typeable
import Data.Maybe (isJust,fromMaybe)
import Data.Monoid (Ap(..))

import qualified Data.Map as M
import qualified Data.Set as S

import XMonad.River.Connection (Connection)
import XMonad.River.Keysym
import XMonad.River.Runtime (sendRestart)
import XMonad.River.Types
import XMonad.River.Wire (ObjectId)

-- | The handle through which the window manager talks to the compositor.
--
-- X11's @Display@ was the connection to the X server, and this is the
-- connection to the Wayland one -- the protocol even calls its root object
-- @wl_display@.  Defining the alias rather than dropping the name is what lets
-- 'withDisplay', 'XMonad.Operations.getCleanedScreenInfo' and
-- 'XMonad.Operations.isFixedSizeOrTransient' keep the signatures they have
-- upstream, so code that merely threads a display through still compiles.
--
-- What does /not/ carry over is anything that called an Xlib function on it.
-- Those names are simply absent, so such code fails at the call that is
-- genuinely unportable rather than here.
type Display = Connection

-- | XState, the (mutable) window manager state.
data XState = XState
    { windowset        :: !WindowSet                     -- ^ workspace list
    , mapped           :: !(S.Set Window)                -- ^ the Set of mapped windows
    , waitingUnmap     :: !(M.Map Window Int)            -- ^ retained for shape; river has no unmap race to track
    , dragging         :: !(Maybe (Position -> Position -> X (), X ()))
    , numberlockMask   :: !KeyMask                       -- ^ retained for shape; river resolves modifiers itself
    , extensibleState  :: !(M.Map String (Either String StateExtension))
    -- ^ stores custom state information.
    --
    -- The module "XMonad.Util.ExtensibleState" in xmonad-contrib
    -- provides additional information and a simple interface for using this.
    }

-- | XConf, the (read-only) window manager configuration.
data XConf = XConf
    { display       :: !Display        -- ^ the compositor connection
    , config        :: !(XConfig Layout)       -- ^ initial user configuration
    , riverManager  :: !ObjectId               -- ^ the @river_window_manager_v1@ global
    , riverBindings :: !ObjectId               -- ^ the @river_xkb_bindings_v1@ global
    , riverCompositor :: !(Maybe ObjectId)
      -- ^ the @wl_compositor@ global, for surfaces the window manager draws
      -- itself.  'Nothing' on a compositor that does not advertise one, which
      -- should not happen but is not worth crashing over -- a session without
      -- decorations beats no session.
    , riverShm      :: !(Maybe ObjectId)
      -- ^ the @wl_shm@ global, for the buffers those surfaces are drawn into.
    , riverWindows  :: !(IORef (M.Map ObjectId RiverWindow))
    , riverOutputs  :: !(IORef (M.Map ObjectId RiverOutput))
    , riverSeats    :: !(IORef (M.Map ObjectId RiverSeat))
    , riverDirty    :: !(IORef Bool)
      -- ^ set when state changed outside a manage sequence, so that one must
      -- be requested with @manage_dirty@
    , inManageSeq   :: !(IORef Bool)
      -- ^ guards requests river only permits during a manage sequence
    , riverRestart  :: !(IORef (Maybe String))
      -- ^ command to exec once river confirms this window manager has stopped
    , riverDragOrigin :: !(IORef (Position, Position))
      -- ^ Where the pointer was when the current interactive operation began.
      -- river reports a drag as a delta from its start; 'mouseDrag' promises
      -- its caller an absolute position, so the origin has to be remembered.
    , normalBorder  :: !BorderColor   -- ^ border colour of unfocused windows
    , focusedBorder :: !BorderColor   -- ^ border colour of the focused window
    , keyActions    :: !(M.Map (KeyMask, KeySym) (X ()))
                                      -- ^ a mapping of key presses to actions
    , buttonActions :: !(M.Map (KeyMask, Button) (Window -> X ()))
                                      -- ^ a mapping of button presses to actions
    , mouseFocused :: !Bool           -- ^ was refocus caused by mouse action?
    , mousePosition :: !(Maybe (Position, Position))
                                      -- ^ position of the mouse according to
                                      -- the event currently being processed
    , currentEvent :: !(Maybe Event)  -- ^ event currently being processed
    , directories  :: !Directories    -- ^ directories to use
    }

-- todo, better name
data XConfig l = XConfig
    { normalBorderColor  :: !String              -- ^ Non focused windows border color. Default: \"#dddddd\"
    , focusedBorderColor :: !String              -- ^ Focused windows border color. Default: \"#ff0000\"
    , terminal           :: !String              -- ^ The preferred terminal application. Default: \"foot\"
    , layoutHook         :: !(l Window)          -- ^ The available layouts
    , manageHook         :: !ManageHook          -- ^ The action to run when a new window is opened
    , handleEventHook    :: !(Event -> X All)    -- ^ Handle a river event, returns (All True) if the default handler
                                                 -- should also be run afterwards. mappend should be used for combining
                                                 -- event hooks in most cases.
    , workspaces         :: ![String]            -- ^ The list of workspaces' names
    , modMask            :: !KeyMask             -- ^ the mod modifier
    , keys               :: !(XConfig Layout -> M.Map (ButtonMask,KeySym) (X ()))
                                                 -- ^ The key binding: a map from key presses and actions
    , mouseBindings      :: !(XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ()))
                                                 -- ^ The mouse bindings
    , borderWidth        :: !Dimension           -- ^ The border width
    , logHook            :: !(X ())              -- ^ The action to perform when the windows set is changed
    , startupHook        :: !(X ())              -- ^ The action to perform on startup
    , focusFollowsMouse  :: !Bool                -- ^ Whether window entry events can change focus
    , clickJustFocuses   :: !Bool                -- ^ False to make a click which changes focus to be additionally passed to the window
    , handleExtraArgs    :: !([String] -> XConfig Layout -> IO (XConfig Layout))
                                                 -- ^ Modify the configuration, complain about extra arguments etc. with arguments that are not handled by default
    , extensibleConf     :: !(M.Map TypeRep ConfExtension)
                                                 -- ^ Stores custom config information.
                                                 --
                                                 -- The module "XMonad.Util.ExtensibleConf" in xmonad-contrib
                                                 -- provides additional information and a simple interface for using this.
    }


-- | The modifier masks, with X11's values -- which are also river's.
--
-- @river_seat_v1.modifiers@ assigns shift=1, ctrl=4, mod1=8, mod3=32, mod4=64,
-- mod5=128, exactly matching @ShiftMask@ and friends.  This is not a
-- coincidence to be grateful for so much as the reason the port is possible at
-- all: it means @mod4Mask@ keeps both its value and its meaning, and a keymap
-- moves across as data.
--
-- 'lockMask' and 'mod2Mask' are the exception.  river has no bit for either --
-- caps lock and num lock are resolved before the window manager sees a
-- binding -- so they keep their X11 values for arithmetic that combines masks,
-- but no binding will ever match on them.  'XMonad.Operations.cleanMask' is
-- the identity here for the same reason.
shiftMask, lockMask, controlMask, mod1Mask, mod2Mask, mod3Mask, mod4Mask,
  mod5Mask, noModMask :: KeyMask
shiftMask   = 1
lockMask    = 2
controlMask = 4
mod1Mask    = 8
mod2Mask    = 16
mod3Mask    = 32
mod4Mask    = 64
mod5Mask    = 128
noModMask   = 0

-- | X11 button numbers.
--
-- river's pointer bindings take Linux input event codes instead, so these are
-- translated at the point of use rather than being the same numbers.  They
-- keep X11's spelling because that is what a config writes.
button1, button2, button3, button4, button5 :: Button
button1 = 1
button2 = 2
button3 = 3
button4 = 4
button5 = 5

type WindowSet   = StackSet  WorkspaceId (Layout Window) Window ScreenId ScreenDetail
type WindowSpace = Workspace WorkspaceId (Layout Window) Window

-- | Virtual workspace indices
type WorkspaceId = String

-- | Physical screen indices
newtype ScreenId    = S Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

-- | The 'Rectangle' with screen dimensions
newtype ScreenDetail = SD { screenRect :: Rectangle }
    deriving (Eq,Show, Read)

------------------------------------------------------------------------

-- | The X monad, 'ReaderT' and 'StateT' transformers over 'IO'
-- encapsulating the window manager configuration and state,
-- respectively.
--
-- Dynamic components may be retrieved with 'get', static components
-- with 'ask'. With newtype deriving we get readers and state monads
-- instantiated on 'XConf' and 'XState' automatically.
--
newtype X a = X (ReaderT XConf (StateT XState IO) a)
    deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadState XState, MonadReader XConf)
    deriving (Semigroup, Monoid) via Ap X a

instance Default a => Default (X a) where
    def = return def

type ManageHook = Query (Endo WindowSet)
newtype Query a = Query (ReaderT Window X a)
    deriving (Functor, Applicative, Monad, MonadReader Window, MonadIO)
    deriving (Semigroup, Monoid) via Ap Query a

runQuery :: Query a -> Window -> X a
runQuery (Query m) = runReaderT m

instance Default a => Default (Query a) where
    def = return def

-- | Run the 'X' monad, given a chunk of 'X' monad code, and an initial state
-- Return the result, and final state
runX :: XConf -> XState -> X a -> IO (a, XState)
runX c st (X a) = runStateT (runReaderT a c) st

-- | Run in the 'X' monad, and in case of exception, and catch it and log it
-- to stderr, and run the error case.
catchX :: X a -> X a -> X a
catchX job errcase = do
    st <- get
    c <- ask
    (a, s') <- io $ runX c st job `E.catch` \e -> case fromException e of
                        Just (_ :: ExitCode) -> throw e
                        _ -> do hPrint stderr e; runX c st errcase
    put s'
    return a

-- | Execute the argument, catching all exceptions.  Either this function or
-- 'catchX' should be used at all callsites of user customized code.
userCode :: X a -> X (Maybe a)
userCode a = catchX (Just <$> a) (return Nothing)

-- | Same as userCode but with a default argument to return instead of using
-- Maybe, provided for convenience.
userCodeDef :: a -> X a -> X a
userCodeDef defValue a = fromMaybe defValue <$> userCode a

-- ---------------------------------------------------------------------
-- Convenient wrappers to state

-- | Run a monad action with the current display settings
withDisplay :: (Display -> X a) -> X a
withDisplay   f = asks display >>= f

-- | Run a monadic action with the current stack set
withWindowSet :: (WindowSet -> X a) -> X a
withWindowSet f = gets windowset >>= f

-- | True if the given window is the root window.
--
-- Always 'False'.  That is not a stub: river has no root window, so nothing is
-- it, and the question has a correct total answer rather than an unavailable
-- one.  Every window river reports is a real toplevel, which is precisely why
-- the X11 callers of this -- "is this event about the root or about a client"
-- -- have nothing to disambiguate here.
isRoot :: Window -> X Bool
isRoot _ = pure False

------------------------------------------------------------------------
-- LayoutClass handling. See particular instances in Operations.hs

-- | An existential type that can hold any object that is in 'Read'
--   and 'LayoutClass'.
data Layout a = forall l. (LayoutClass l a, Read (l a)) => Layout (l a)

-- | Using the 'Layout' as a witness, parse existentially wrapped windows
-- from a 'String'.
readsLayout :: Layout a -> String -> [(Layout a, String)]
readsLayout (Layout l) s = [(Layout (asTypeOf x l), rs) | (x, rs) <- reads s]

-- | Every layout must be an instance of 'LayoutClass', which defines
-- the basic layout operations along with a sensible default for each.
--
-- All of the methods have default implementations, so there is no
-- minimal complete definition.  They do, however, have a dependency
-- structure by default; this is something to be aware of should you
-- choose to implement one of these methods.  Here is how a minimal
-- complete definition would look like if we did not provide any default
-- implementations:
--
-- * 'runLayout' || (('doLayout' || 'pureLayout') && 'emptyLayout')
--
-- * 'handleMessage' || 'pureMessage'
--
-- * 'description'
--
-- Note that any code which /uses/ 'LayoutClass' methods should only
-- ever call 'runLayout', 'handleMessage', and 'description'!
class (Show (layout a), Typeable layout) => LayoutClass layout a where

    -- | By default, 'runLayout' calls 'doLayout' if there are any
    --   windows to be laid out, and 'emptyLayout' otherwise.
    runLayout :: Workspace WorkspaceId (layout a) a
              -> Rectangle
              -> X ([(a, Rectangle)], Maybe (layout a))
    runLayout (Workspace _ l ms) r = maybe (emptyLayout l r) (doLayout l r) ms

    -- | Given a 'Rectangle' in which to place the windows, and a 'Stack'
    -- of windows, return a list of windows and their corresponding
    -- Rectangles.  The order of windows in this list should be the
    -- desired stacking order.
    doLayout    :: layout a -> Rectangle -> Stack a
                -> X ([(a, Rectangle)], Maybe (layout a))
    doLayout l r s   = return (pureLayout l r s, Nothing)

    -- | This is a pure version of 'doLayout'.
    pureLayout  :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
    pureLayout _ r s = [(focus s, r)]

    -- | 'emptyLayout' is called when there are no windows.
    emptyLayout :: layout a -> Rectangle -> X ([(a, Rectangle)], Maybe (layout a))
    emptyLayout _ _ = return ([], Nothing)

    -- | 'handleMessage' performs message handling.
    handleMessage :: layout a -> SomeMessage -> X (Maybe (layout a))
    handleMessage l  = return . pureMessage l

    -- | Respond to a message by (possibly) changing our layout, but
    -- taking no other action.
    pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
    pureMessage _ _  = Nothing

    -- | This should be a human-readable string that is used when
    -- selecting layouts by name.
    description :: layout a -> String
    description      = show

instance LayoutClass Layout Window where
    runLayout (Workspace i (Layout l) ms) r = fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
    doLayout (Layout l) r s  = fmap (fmap Layout) `fmap` doLayout l r s
    emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
    handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
    description (Layout l)   = description l

instance Show (Layout a) where show (Layout l) = show l

-- | Based on ideas in /An Extensible Dynamically-Typed Hierarchy of
-- Exceptions/, Simon Marlow, 2006. Use extensible messages to the
-- 'handleMessage' handler.
--
-- User-extensible messages must be a member of this class.
--
class Typeable a => Message a

-- |
-- A wrapped value of some type in the 'Message' class.
--
data SomeMessage = forall a. Message a => SomeMessage a

-- |
-- And now, unwrap a given, unknown 'Message' type, performing a (dynamic)
-- type check on the result.
--
fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- River events are valid Messages, exactly as X events were.
instance Message Event

-- | 'LayoutMessages' are core messages that all layouts (especially stateful
-- layouts) should consider handling.
data LayoutMessages = Hide              -- ^ sent when a layout becomes non-visible
                    | ReleaseResources  -- ^ sent when xmonad is exiting or restarting
    deriving Eq

instance Message LayoutMessages

-- ---------------------------------------------------------------------
-- Extensible state/config
--

-- | Every module must make the data it wants to store
-- an instance of this class.
--
-- Minimal complete definition: initialValue
class Typeable a => ExtensionClass a where
    {-# MINIMAL initialValue #-}
    -- | Defines an initial value for the state extension
    initialValue :: a
    -- | Specifies whether the state extension should be
    -- persistent.
    --
    -- Note that under river this makes no difference: there is no state file,
    -- because a window id serialised by one process means nothing to its
    -- successor.  'PersistentExtension' therefore behaves exactly like
    -- 'StateExtension'.  See README.river.md.
    extensionType :: a -> StateExtension
    extensionType = StateExtension

-- | Existential type to store a state extension.
data StateExtension =
    forall a. ExtensionClass a => StateExtension a
    -- ^ Non-persistent state extension
  | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a
    -- ^ Persistent extension

-- | Existential type to store a config extension.
data ConfExtension = forall a. Typeable a => ConfExtension a

-- ---------------------------------------------------------------------
-- General utilities

-- | If-then-else lifted to a 'Monad'.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = mb >>= \b -> if b then t else f

-- | Lift an 'IO' action into the 'X' monad
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Lift an 'IO' action into the 'X' monad.  If the action results in an 'IO'
-- exception, log the exception to stderr and continue normal execution.
catchIO :: MonadIO m => IO () -> m ()
catchIO f = io (f `E.catch` \(SomeException e) -> hPrint stderr e >> hFlush stderr)

-- | spawn. Launch an external application. Specifically, it double-forks and
-- runs the 'String' you pass as a command to \/bin\/sh.
--
-- Note this function assumes your locale uses utf8.
spawn :: MonadIO m => String -> m ()
spawn x = void $ spawnPID x

-- | Like 'spawn', but returns the 'ProcessID' of the launched application
spawnPID :: MonadIO m => String -> m ProcessID
spawnPID x = xfork $ executeFile "/bin/sh" False ["-c", x] Nothing

-- | A replacement for 'forkProcess' which resets default signal handlers.
xfork :: MonadIO m => IO () -> m ProcessID
xfork x = io . forkProcess . finally nullStdin $ do
                uninstallSignalHandlers
                createSession
                x
 where
    nullStdin = do
#if MIN_VERSION_unix(2,8,0)
        fd <- openFd "/dev/null" ReadOnly defaultFileFlags
#else
        fd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
#endif
        dupTo fd stdInput
        closeFd fd

-- | This is basically a map function, running a function in the 'X' monad on
-- each workspace with the output of that function being the modified workspace.
runOnWorkspaces :: (WindowSpace -> X WindowSpace) -> X ()
runOnWorkspaces job = do
    ws <- gets windowset
    h <- mapM job $ hidden ws
    c:v <- mapM (\s -> (\w -> s { workspace = w}) <$> job (workspace s))
             $ current ws : visible ws
    modify $ \s -> s { windowset = ws { current = c, visible = v, hidden = h } }

-- | All the directories that xmonad will use.  They will be used for
-- the following purposes:
--
-- * @dataDir@: This directory is used by XMonad to store data files
-- such as the run-time state file.
--
-- * @cfgDir@: This directory is where user configuration files are
-- stored (e.g, the xmonad.hs file).  You may also create a @lib@
-- subdirectory in the configuration directory and the default recompile
-- command will add it to the GHC include path.
--
-- * @cacheDir@: This directory is used to store temporary files that
-- can easily be recreated such as the configuration binary and any
-- intermediate object files generated by GHC.
--
-- For how these directories are chosen, see 'getDirectories'.
--
data Directories' a = Directories
    { dataDir  :: !a
    , cfgDir   :: !a
    , cacheDir :: !a
    }
    deriving (Show, Functor, Foldable, Traversable)

-- | Convenient type alias for the most common case in which one might
-- want to use the 'Directories' type.
type Directories = Directories' FilePath

-- | Build up the 'Dirs' that xmonad will use.  They are chosen as
-- follows:
--
-- 1. If all three of xmonad's environment variables (@XMONAD_DATA_DIR@,
--    @XMONAD_CONFIG_DIR@, and @XMONAD_CACHE_DIR@) are set, use them.
-- 2. If there is a build script called @build@ or configuration
--    @xmonad.hs@ in @~\/.xmonad@, set all three directories to
--    @~\/.xmonad@.
-- 3. Otherwise, use the @xmonad@ directory in @XDG_DATA_HOME@,
--    @XDG_CONFIG_HOME@, and @XDG_CACHE_HOME@ (or their respective
--    fallbacks).  These directories are created if necessary.
--
getDirectories :: IO Directories
getDirectories = xmEnvDirs <|> xmDirs <|> xdgDirs
  where
    -- | Check for xmonad's environment variables first
    xmEnvDirs :: IO Directories
    xmEnvDirs = do
        let xmEnvs = Directories{ dataDir  = "XMONAD_DATA_DIR"
                                , cfgDir   = "XMONAD_CONFIG_DIR"
                                , cacheDir = "XMONAD_CACHE_DIR"
                                }
        maybe empty pure . sequenceA =<< traverse getEnv xmEnvs

    -- | Check whether the config file or a build script is in the
    -- @~\/.xmonad@ directory
    xmDirs :: IO Directories
    xmDirs = do
        xmDir <- getAppUserDataDirectory "xmonad"
        conf  <- doesFileExist $ xmDir </> "xmonad.hs"
        build <- doesFileExist $ xmDir </> "build"

        -- Place *everything* in ~/.xmonad if yes
        guard $ conf || build
        pure Directories{ dataDir = xmDir, cfgDir = xmDir, cacheDir = xmDir }

    -- | Use XDG directories as a fallback
    xdgDirs :: IO Directories
    xdgDirs =
        for Directories{ dataDir = XdgData, cfgDir = XdgConfig, cacheDir = XdgCache }
            $ \dir -> do d <- getXdgDirectory dir "xmonad"
                         d <$ createDirectoryIfMissing True d

-- | Return the path to the xmonad configuration directory.
getXMonadDir :: X String
getXMonadDir = asks (cfgDir . directories)
{-# DEPRECATED getXMonadDir "Use `asks (cfgDir . directories)' instead." #-}

-- | Return the path to the xmonad cache directory.
getXMonadCacheDir :: X String
getXMonadCacheDir = asks (cacheDir . directories)
{-# DEPRECATED getXMonadCacheDir "Use `asks (cacheDir . directories)' instead." #-}

-- | Return the path to the xmonad data directory.
getXMonadDataDir :: X String
getXMonadDataDir = asks (dataDir . directories)
{-# DEPRECATED getXMonadDataDir "Use `asks (dataDir . directories)' instead." #-}

binFileName, buildDirName :: Directories -> FilePath
binFileName  Directories{ cacheDir } = cacheDir </> "xmonad-" <> arch <> "-" <> os
buildDirName Directories{ cacheDir } = cacheDir </> "build-" <> arch <> "-" <> os

errFileName :: Directories -> FilePath
errFileName   Directories{ dataDir } = dataDir </> "xmonad.errors"

srcFileName, libFileName :: Directories -> FilePath
srcFileName Directories{ cfgDir } = cfgDir </> "xmonad.hs"
libFileName Directories{ cfgDir } = cfgDir </> "lib"

buildScriptFileName, stackYamlFileName, nixFlakeFileName, nixDefaultFileName :: Directories -> FilePath
buildScriptFileName Directories{ cfgDir } = cfgDir </> "build"
stackYamlFileName   Directories{ cfgDir } = cfgDir </> "stack.yaml"
nixFlakeFileName    Directories{ cfgDir } = cfgDir </> "flake.nix"
nixDefaultFileName  Directories{ cfgDir } = cfgDir </> "default.nix"

-- | Compilation method for xmonad configuration.
data Compile
  = CompileGhc
  | CompileCabal
  | CompileStackGhc FilePath
  | CompileNixFlake
  | CompileNixDefault
  | CompileScript FilePath
    deriving (Show)

-- | Detect compilation method by looking for known file names in xmonad
-- configuration directory.
detectCompile :: Directories -> IO Compile
detectCompile dirs =
  tryScript <|> tryStack <|> tryNixFlake <|> tryNixDefault <|> tryCabal <|> useGhc
  where
    buildScript = buildScriptFileName dirs
    stackYaml = stackYamlFileName dirs
    flakeNix = nixFlakeFileName dirs
    defaultNix = nixDefaultFileName dirs

    tryScript = do
        guard =<< doesFileExist buildScript
        isExe <- isExecutable buildScript
        if isExe
          then do
            trace $ "XMonad will use build script at " <> show buildScript <> " to recompile."
            pure $ CompileScript buildScript
          else do
            trace $ "XMonad will not use build script, because " <> show buildScript <> " is not executable."
            trace $ "Suggested resolution to use it: chmod u+x " <> show buildScript
            empty

    tryNixFlake = do
      guard =<< doesFileExist flakeNix
      canonNixFlake <- canonicalizePath flakeNix
      trace $ "XMonad will use nix flake at " <> show canonNixFlake <> " to recompile"
      pure CompileNixFlake

    tryNixDefault = do
      guard =<< doesFileExist defaultNix
      canonNixDefault <- canonicalizePath defaultNix
      trace $ "XMonad will use nix file at " <> show canonNixDefault <> " to recompile"
      pure CompileNixDefault

    tryStack = do
        guard =<< doesFileExist stackYaml
        canonStackYaml <- canonicalizePath stackYaml
        trace $ "XMonad will use stack ghc --stack-yaml " <> show canonStackYaml <> " to recompile."
        pure $ CompileStackGhc canonStackYaml

    tryCabal = let cwd = cfgDir dirs in listCabalFiles cwd >>= \ case
        [] -> do
            empty
        [name] -> do
            trace $ "XMonad will use " <> show name <> " to recompile."
            pure CompileCabal
        _ : _ : _ -> do
            trace $ "XMonad will not use cabal, because there are multiple cabal files in " <> show cwd <> "."
            empty

    useGhc = do
        trace $ "XMonad will use ghc to recompile, because none of "
                <> intercalate ", "
                     [ show buildScript
                     , show stackYaml
                     , show flakeNix
                     , show defaultNix
                     ] <> " nor a suitable .cabal file exist."
        pure CompileGhc

listCabalFiles :: FilePath -> IO [FilePath]
listCabalFiles dir = map (dir </>) . Prelude.filter isCabalFile <$> listFiles dir

isCabalFile :: FilePath -> Bool
isCabalFile file = case splitExtension file of
    (name, ".cabal") -> not (null name)
    _ -> False

listFiles :: FilePath -> IO [FilePath]
listFiles dir = getDirectoryContents dir >>= filterM (doesFileExist . (dir </>))

-- | Determine whether or not the file found at the provided filepath is executable.
isExecutable :: FilePath -> IO Bool
isExecutable f = E.catch (executable <$> getPermissions f) (\(SomeException _) -> return False)

-- | Should we recompile xmonad configuration? Is it newer than the compiled
-- binary?
shouldCompile :: Directories -> Compile -> IO Bool
shouldCompile dirs CompileGhc = do
    libTs <- mapM getModTime . Prelude.filter isSource =<< allFiles (libFileName dirs)
    srcT <- getModTime (srcFileName dirs)
    binT <- getModTime (binFileName dirs)
    if any (binT <) (srcT : libTs)
        then True <$ trace "XMonad recompiling because some files have changed."
        else False <$ trace "XMonad skipping recompile because it is not forced (e.g. via --recompile), and neither xmonad.hs nor any *.hs / *.lhs / *.hsc files in lib/ have been changed."
  where
    isSource = flip elem [".hs",".lhs",".hsc"] . takeExtension
    allFiles t = do
        let prep = map (t</>) . Prelude.filter (`notElem` [".",".."])
        cs <- prep <$> E.catch (getDirectoryContents t) (\(SomeException _) -> return [])
        ds <- filterM doesDirectoryExist cs
        concat . ((cs \\ ds):) <$> mapM allFiles ds
shouldCompile _ CompileCabal = return True
shouldCompile dirs CompileStackGhc{} = do
    stackYamlT <- getModTime (stackYamlFileName dirs)
    binT <- getModTime (binFileName dirs)
    if binT < stackYamlT
        then True <$ trace "XMonad recompiling because some files have changed."
        else shouldCompile dirs CompileGhc
shouldCompile _dirs CompileNixFlake{} = True <$ trace "XMonad recompiling because flake recompilation is being used."
shouldCompile _dirs CompileNixDefault{} = True <$ trace "XMonad recompiling because nix recompilation is being used."
shouldCompile _dirs CompileScript{} =
    True <$ trace "XMonad recompiling because a custom build script is being used."

getModTime :: FilePath -> IO (Maybe UTCTime)
getModTime f = E.catch (Just <$> getModificationTime f) (\(SomeException _) -> return Nothing)

-- | Compile the configuration.
compile :: Directories -> Compile -> IO ExitCode
compile dirs method =
    bracket_ uninstallSignalHandlers installSignalHandlers $
        withFile (errFileName dirs) WriteMode $ \err -> do
            let run = runProc err
            case method of
                CompileGhc -> do
                    ghc <- fromMaybe "ghc" <$> lookupEnv "XMONAD_GHC"
                    run ghc ghcArgs
                CompileCabal -> run "cabal" ["build"] .&&. copyBinary
                  where
                    copyBinary :: IO ExitCode
                    copyBinary = readProc err "cabal" ["-v0", "list-bin", "."] >>= \ case
                        Left status -> return status
                        Right (trim -> path) -> copyBinaryFrom path
                CompileStackGhc stackYaml ->
                    run "stack" ["build", "--silent", "--stack-yaml", stackYaml] .&&.
                    run "stack" ("ghc" : "--stack-yaml" : stackYaml : "--" : ghcArgs)
                CompileNixFlake ->
                    run "nix" ["build"] >>= andCopyFromResultDir
                CompileNixDefault ->
                    run "nix-build" [] >>= andCopyFromResultDir
                CompileScript script ->
                    run script [binFileName dirs]
  where
    cwd :: FilePath
    cwd = cfgDir dirs

    ghcArgs :: [String]
    ghcArgs = [ "--make"
              , "xmonad.hs"
              , "-i" -- only look in @lib@
              , "-ilib"
              , "-fforce-recomp"
              , "-main-is", "main"
              , "-v0"
              , "-outputdir", buildDirName dirs
              , "-o", binFileName dirs
              ]

    andCopyFromResultDir :: ExitCode -> IO ExitCode
    andCopyFromResultDir exitCode = do
      if exitCode == ExitSuccess then copyFromResultDir else return exitCode

    findM :: (Monad m, Foldable t) => (a -> m Bool) -> t a -> m (Maybe a)
    findM p = foldr (\x -> ifM (p x) (pure $ Just x)) (pure Nothing)

    catchAny :: IO a -> (SomeException -> IO a) -> IO a
    catchAny = E.catch

    copyFromResultDir :: IO ExitCode
    copyFromResultDir = do
      let binaryDirectory = cfgDir dirs </> "result" </> "bin"
      binFiles <- map (binaryDirectory </>) <$> catchAny (listDirectory binaryDirectory) (\_ -> return [])
      mfilepath <- findM isExecutable binFiles
      case mfilepath of
        Just filepath -> copyBinaryFrom filepath
        Nothing -> return $ ExitFailure 1

    copyBinaryFrom :: FilePath -> IO ExitCode
    copyBinaryFrom filepath = copyFile filepath (binFileName dirs) >> return ExitSuccess

    -- waitForProcess =<< System.Process.runProcess, but without closing the err handle
    runProc :: Handle -> String -> [String] -> IO ExitCode
    runProc err exe args = do
        (Nothing, Nothing, Nothing, h) <- createProcess_ "runProc" =<< mkProc err exe args
        waitForProcess h

    readProc :: Handle -> String -> [String] -> IO (Either ExitCode String)
    readProc err exe args = do
        spec <- mkProc err exe args
        (Nothing, Just out, Nothing, h) <- createProcess_ "readProc" spec{ std_out = CreatePipe }
        result <- hGetContents out
        hPutStr err result >> hFlush err
        waitForProcess h >>= \ case
            ExitSuccess -> return $ Right result
            status -> return $ Left status

    mkProc :: Handle -> FilePath -> [FilePath] -> IO CreateProcess
    mkProc err exe args = do
        hPutStrLn err $ unwords $ "$" : exe : args
        hFlush err
        return (proc exe args){ cwd = Just cwd, std_err = UseHandle err }

    (.&&.) :: Monad m => m ExitCode -> m ExitCode -> m ExitCode
    cmd1 .&&. cmd2 = cmd1 >>= \case
        ExitSuccess -> cmd2
        e -> pure e

-- | Check GHC output for deprecation warnings and notify the user if there
-- were any. Report success otherwise.
--
-- xmonad pops an @xmessage@ here.  That is an X11 client, so under river the
-- report goes to stderr only -- which lands in the journal, and is where the
-- rest of this backend's diagnostics go too.
checkCompileWarnings :: Directories -> IO ()
checkCompileWarnings dirs = do
    ghcErr <- readFile (errFileName dirs)
    if "-Wdeprecations" `isInfixOf` ghcErr
      then do
        let msg = unlines $
                ["Deprecations detected while compiling xmonad config: " <> srcFileName dirs]
                ++ lines ghcErr
                ++ ["","Please correct them or silence using {-# OPTIONS_GHC -Wno-deprecations #-}."]
        trace msg
      else
        trace "XMonad recompilation process exited with success!"

-- | Notify the user that compilation failed and what was wrong.
compileFailed :: Directories -> ExitCode -> IO ()
compileFailed dirs status = do
    ghcErr <- readFile (errFileName dirs)
    let msg = unlines $
            ["Errors detected while compiling xmonad config: " <> srcFileName dirs]
            ++ lines (if null ghcErr then show status else ghcErr)
            ++ ["","Please check the file for errors."]
    trace msg

-- | Recompile the xmonad configuration file when any of the following apply:
--
--  * force is 'True'
--
--  * the xmonad executable does not exist
--
--  * the xmonad executable is older than @xmonad.hs@ or any file in
--    the @lib@ directory (under the configuration directory)
--
--  * custom @build@ script is being used
--
-- Compilation errors (if any) are logged to the @xmonad.errors@ file
-- in the xmonad data directory.
--
-- 'False' is returned if there are compilation errors.
--
recompile :: MonadIO m => Directories -> Bool -> m Bool
recompile dirs force = io $ do
    method <- detectCompile dirs
    willCompile <- if force
        then True <$ trace "XMonad recompiling (forced)."
        else shouldCompile dirs method
    if willCompile
      then do
        status <- compile dirs method
        if status == ExitSuccess
            then checkCompileWarnings dirs
            else compileFailed dirs status
        pure $ status == ExitSuccess
      else
        pure True

-- | Conditionally run an action, using a @Maybe a@ to decide.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

-- | Conditionally run an action, using a 'X' event to decide
whenX :: X Bool -> X () -> X ()
whenX a f = a >>= \b -> when b f

-- | A 'trace' for the 'X' monad. Logs a string to stderr, and therefore into
-- the journal.
trace :: MonadIO m => String -> m ()
trace = io . hPutStrLn stderr

-- | Ignore SIGPIPE to avoid termination when a pipe is full, and SIGCHLD to
-- avoid zombie processes, and clean up any extant zombie processes.
installSignalHandlers :: MonadIO m => m ()
installSignalHandlers = io $ do
    installHandler openEndedPipe Ignore Nothing
    -- Note: mgsloan modification to allow for waiting for processes
    -- installHandler sigCHLD Ignore Nothing
    (try :: IO a -> IO (Either SomeException a))
      $ fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    return ()

uninstallSignalHandlers :: MonadIO m => m ()
uninstallSignalHandlers = io $ do
    installHandler openEndedPipe Default Nothing
    -- Note: mgsloan modification to allow for waiting for processes
    -- installHandler sigCHLD Default Nothing
    return ()

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
