{-# LANGUAGE LambdaCase #-}
-- | A window manager with a submap in its config, and a keyboard to press.
--
-- What this exists to catch: a submap disables every one of the window
-- manager's bindings while it is open, so the way it fails is a session in
-- which no shortcut works at all.  Nothing else in the suite covers that.
-- "XMonad.River.WM" and "XMonad.River" both carry comments warning about it,
-- which is a reasonable sign it is worth a test rather than a comment.
--
-- Two things make this awkward enough to explain.
--
-- __It is a window manager, not a client.__  A submap is made of bindings, and
-- only the window manager has any.  So this links the library and calls
-- 'launch' with a config of its own, exactly as somebody's @xmonad.hs@ does --
-- the stock binary's config has no submap in it, and adding one for a test
-- would be a test fixture living in production code.
--
-- __A headless seat has no keyboard.__  River matches bindings against a
-- seat's keyboard state, and a headless one has none, so the presses have to
-- come from somewhere.  @virtual-keyboard-unstable-v1@ is that somewhere: a
-- second connection, an ordinary client on it, giving the seat a keyboard and
-- then typing.  This is the gap @tests\/headless-prompt.sh@ notes it cannot
-- cover; the bindings for it are generated now, so it is covered here.
--
-- The assertions are in @tests\/headless-submap.sh@, which reads the log this
-- writes.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.Word (Word32)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO
import System.Posix.IO (OpenMode (ReadWrite), defaultFileFlags, openFd)
import qualified Control.Exception as E
import qualified Data.Map as M

import XMonad
import XMonad.River (submapNextKey)
import XMonad.River.Connection
import XMonad.River.Wire (ObjectId)
import XMonad.River.Protocol.Core (wlSeatInterface, wlSeatVersion)
import XMonad.River.Protocol.VirtualKeyboard
import XMonad.River.Xkb (defaultKeymapText)

-- | Where the config's actions record that they ran.
logPathEnv :: String
logPathEnv = "XMONAD_SUBMAP_LOG"

note :: FilePath -> String -> X ()
note path msg = io $ do
  appendFile path (msg ++ "\n")
  hPutStrLn stderr ("river-submap-spec: " ++ msg)

-- | Everything the keyboard will be asked to press, as evdev codes.
--
-- Evdev rather than xkb keycodes: @zwp_virtual_keyboard_v1.key@ takes the
-- former, which is the latter minus 8.
keyM, keyA, keyB, keyZ, keyEsc :: Word32
keyM = 50
keyA = 30
keyB = 48
keyZ = 44
keyEsc = 1

-- | @Mod4@, as libxkbcommon numbers it in the default keymap -- the same
-- coincidence 'XMonad.River.Keyboard.riverModifiers' rests on.
mod4Bit :: Word32
mod4Bit = 64

-- | @Shift|Control|Mod1@: the panic chord's modifiers.
panicBits :: Word32
panicBits = 1 + 4 + 8

main :: IO ()
main = do
  path <- lookupEnv logPathEnv >>= \case
    Just p  -> pure p
    Nothing -> hPutStrLn stderr
      ("river-submap-spec: " ++ logPathEnv ++ " is not set") >> exitFailure
  writeFile path ""
  dirs <- getDirectories

  -- Before 'launch', which never returns.
  void (forkIO (E.handle report (drive path)))

  launch (testConfig path) dirs
  where
    report :: E.SomeException -> IO ()
    report e = hPutStrLn stderr ("river-submap-spec: keyboard: " ++ show e)

-- | The config under test.
--
-- @M-m@ opens a submap in which @a@ runs something and anything else is
-- unbound; @M-b@ is an ordinary global binding, and is the one that matters --
-- if it still works after the submap has closed, the bindings a submap
-- disabled were restored.  @M-z@ takes longer than the loop waits for a plan,
-- so its effect lands a sequence late; nothing about it may be lost.
testConfig :: FilePath -> XConfig (Choose Tall (Choose (Mirror Tall) Full))
testConfig path = def
  { modMask = mod4Mask
  , keys = \_ -> M.fromList
      [ ( (mod4Mask, xK_m)
        , submapNextKey (M.fromList [((0, xK_a), note path "inner-a")])
                        (note path "inner-unbound") )
      , ((mod4Mask, xK_b), note path "global-b")
      , ((mod4Mask, xK_z), io (threadDelay (80 * 1000)) >> note path "slow-z")
      ]
  }

--------------------------------------------------------------------------------
-- The keyboard

-- | Give the seat a keyboard, then type.
--
-- On a connection of its own, because this is a client and the window manager
-- is not.  'XMonad.River.Client' makes the same move for prompts and for the
-- same reason.
drive :: FilePath -> IO ()
drive path = do
  -- Long enough for the window manager to have finished its first manage
  -- sequence, which is when the config's bindings are created.  Nothing
  -- observable says when that has happened, so this is a sleep; the assertions
  -- fail loudly rather than silently if it is too short.
  threadDelay (4 * 1000 * 1000)

  conn <- connect
  (reg, globals) <- getRegistry conn
  mSeat <- bindGlobal conn reg globals wlSeatInterface 1 wlSeatVersion
  mMgr <- bindGlobal conn reg globals
            zwpVirtualKeyboardManagerV1Interface 1 zwpVirtualKeyboardManagerV1Version
  case (mSeat, mMgr) of
    (Nothing, _) -> hPutStrLn stderr "river-submap-spec: no wl_seat"
    (_, Nothing) -> hPutStrLn stderr
      "river-submap-spec: no zwp_virtual_keyboard_manager_v1; is river built \
      \without it?"
    (Just (seat, _), Just (mgr, _)) -> do
      kb <- zwpVirtualKeyboardManagerV1CreateVirtualKeyboard conn mgr seat
      uploadKeymap conn kb path
      roundtrip conn

      -- 1. M-m opens the submap.
      chord conn kb mod4Bit keyM
      settle conn
      -- 2. a is in it: this runs, and the submap tears down.
      tap conn kb keyA
      settle conn
      -- 3. M-b is a global.  It only fires if the submap put the globals back.
      chord conn kb mod4Bit keyB
      settle conn

      -- The other exit: open a submap and press something it does not want.
      chord conn kb mod4Bit keyM
      settle conn
      tap conn kb keyZ
      settle conn
      chord conn kb mod4Bit keyB
      settle conn

      -- The third exit, the panic chord.  It drops the capture without running
      -- anything; the submap's key must then do nothing -- its binding was
      -- destroyed, not merely orphaned -- and the globals must be back.
      chord conn kb mod4Bit keyM
      settle conn
      chord conn kb panicBits keyEsc
      settle conn
      tap conn kb keyA
      settle conn
      chord conn kb mod4Bit keyB
      settle conn

      -- An action slower than the plan grace: the loop answers river with the
      -- plan it has, and the late one follows in a sequence of its own.  The
      -- global after it proves the loop is still answering.
      chord conn kb mod4Bit keyZ
      settle conn
      chord conn kb mod4Bit keyB
      settle conn

-- | Hand the compositor a keymap, which a virtual keyboard must do before it
-- may send a key.
uploadKeymap :: Connection -> ObjectId -> FilePath -> IO ()
uploadKeymap conn kb path = defaultKeymapText >>= \case
  Nothing -> hPutStrLn stderr "river-submap-spec: no xkb keymap available"
  Just text -> do
    let km = path ++ ".keymap"
    writeFile km text
    fd <- openFd km ReadWrite defaultFileFlags
    -- 1 is XKB_KEYMAP_FORMAT_TEXT_V1.  The size includes the terminator, which
    -- libxkbcommon expects to find.
    zwpVirtualKeyboardV1Keymap conn kb 1 fd (fromIntegral (length text + 1))
    -- No closeFd here: 'flush' closes every descriptor it hands over, on the
    -- grounds that the compositor holds its own now.  Closing it again is
    -- EBADF, which as an exception on this thread means no key is ever sent.
    flush conn

-- | Press and release one key with no modifiers.
tap :: Connection -> ObjectId -> Word32 -> IO ()
tap conn kb code = do
  zwpVirtualKeyboardV1Key conn kb 0 code 1
  zwpVirtualKeyboardV1Key conn kb 0 code 0

-- | Press and release one key with a modifier held across it.
chord :: Connection -> ObjectId -> Word32 -> Word32 -> IO ()
chord conn kb mods code = do
  zwpVirtualKeyboardV1Modifiers conn kb mods 0 0 0
  zwpVirtualKeyboardV1Key conn kb 0 code 1
  zwpVirtualKeyboardV1Key conn kb 0 code 0
  zwpVirtualKeyboardV1Modifiers conn kb 0 0 0 0

-- | Let the window manager act on what was just sent.
--
-- A binding's action does not run when the key arrives: it is queued and run
-- at the start of the next manage sequence, which the window manager asks for.
-- So every step here has to wait for a round trip of the /other/ connection,
-- which nothing on this one can observe -- hence a sleep rather than a sync.
settle :: Connection -> IO ()
settle conn = do
  flush conn
  roundtrip conn
  threadDelay (1200 * 1000)
