{-# LANGUAGE LambdaCase #-}

-- | Capturing keys that are not bound to a window manager action.
--
-- A prompt, a submap, or anything else that wants to read typing rather than
-- react to a shortcut needs every keystroke while it is open.  Under X11 that
-- was a keyboard grab: the window manager asked the server to route all keys
-- to it, read them from its event queue, and released the grab afterwards.
--
-- River offers nothing of the sort, and the reason is worth stating because it
-- shapes everything below.  @river_seat_v1@ has no key events at all -- the
-- window manager is told which /binding/ fired, never which key was pressed.
-- Its @pressed@ event carries no arguments; the binding object is the entire
-- message.  @river_xkb_bindings_v1.ensure_next_key_eaten@ comes closest, but
-- its @ate_unbound_key@ event is likewise argument-free: it reports that a key
-- was swallowed without saying which, which is enough to cancel a submap on an
-- unknown key and not enough to type with.
--
-- So identity has to come from the binding object itself, which means creating
-- one binding per key worth reading.  That is what this module does: it builds
-- a binding for every requested @(modifier, keysym)@ pair, remembers which
-- object means which key, and tears the whole set down afterwards.  A hundred
-- or so protocol objects for the lifetime of a prompt sounds extravagant and
-- is not -- they are ids and a listener each, created and destroyed in one
-- round trip.
--
-- The awkwardness this leaves is that the key set must be enumerated in
-- advance.  A prompt cannot say "give me whatever is typed"; it must say which
-- keysyms it is prepared to receive.  For text entry that means naming the
-- printable range, which "XMonad.River.Keyboard.printableKeys" does.
module XMonad.River.Keyboard
  ( KeyGrab
  , grabKeys
  , ungrabKeys
  , printableKeys
  ) where

import Control.Monad (forM, forM_)
import Data.Bits ((.&.))
import Data.IORef
import Data.Word (Word32)
import qualified Data.Map.Strict as M

import XMonad.River.Connection (Connection)
import XMonad.River.Keysym (stringToKeysym)
import XMonad.River.Protocol.WindowManagement
import XMonad.River.Protocol.XkbBindings
import XMonad.River.Types (KeyMask, KeySym)
import XMonad.River.Wire (ObjectId)

-- | A set of bindings held for as long as something is reading keys.
newtype KeyGrab = KeyGrab [ObjectId]

-- | Create a binding for each requested key on each seat, routing presses to
-- the handler.
--
-- The handler runs on the connection's dispatch thread, so it must not block:
-- anything slow belongs on the mailbox.  See "XMonad.River.Mailbox".
--
-- Must be called during a manage sequence.  Bindings are window management
-- state, and river rejects the request outside one.
grabKeys
  :: Connection
  -> ObjectId                     -- ^ the @river_xkb_bindings_v1@ global
  -> [ObjectId]                   -- ^ seats
  -> [(KeyMask, KeySym)]
  -> ((KeyMask, KeySym) -> IO ())
  -> IO KeyGrab
grabKeys conn bindingsGlobal seats keys handler = do
  table <- newIORef M.empty
  objs <- fmap concat . forM seats $ \seat ->
    forM keys $ \k@(mask, keysym) -> do
      b <- riverXkbBindingsV1GetXkbBinding conn bindingsGlobal seat keysym
             (riverModifiers mask)
      modifyIORef' table (M.insert b k)
      riverXkbBindingV1Listen conn b $ \case
        RiverXkbBindingV1Pressed ->
          readIORef table >>= \t -> forM_ (M.lookup b t) handler
        _ -> pure ()
      riverXkbBindingV1Enable conn b
      pure b
  pure (KeyGrab objs)

-- | Release a grab, destroying every binding it created.
ungrabKeys :: Connection -> KeyGrab -> IO ()
ungrabKeys conn (KeyGrab objs) = forM_ objs $ \b -> do
  riverXkbBindingV1Disable conn b
  riverXkbBindingV1Destroy conn b

-- | The keys a text field needs: printable Latin-1, plus editing and
-- navigation.
--
-- Each is offered unmodified and with shift, since river reports the modifier
-- state that was active and a capital letter arrives as shift plus the
-- lowercase keysym.  Control combinations are /not/ included: a prompt that
-- wants @C-a@ has to ask for it, because binding the whole control range would
-- swallow the window manager's own shortcuts for as long as the prompt is
-- open.
printableKeys :: [(KeyMask, KeySym)]
printableKeys =
  [ (m, ks)
  | ks <- [0x20 .. 0x7e] ++ named
  , m <- [0, shiftMask]
  ]
  where
    shiftMask = 1
    named = map stringToKeysym
      [ "Return", "BackSpace", "Delete", "Escape", "Tab"
      , "Left", "Right", "Up", "Down", "Home", "End"
      , "Page_Up", "Page_Down"
      ]

-- | Drop the modifier bits river has no entry for.
--
-- Lock and mod2 are resolved before the window manager sees anything, so a
-- mask containing them would match nothing.
riverModifiers :: KeyMask -> Word32
riverModifiers mask = mask .&. supported
  where
    supported = riverSeatV1ModifiersShift
            + riverSeatV1ModifiersCtrl
            + riverSeatV1ModifiersMod1
            + riverSeatV1ModifiersMod3
            + riverSeatV1ModifiersMod4
            + riverSeatV1ModifiersMod5
