{-# LANGUAGE ForeignFunctionInterface #-}

-- | Turning Wayland keycodes into keysyms and text, via libxkbcommon.
--
-- A @wl_keyboard.key@ event carries a raw evdev keycode and nothing else.
-- Everything a person expects from a keyboard -- that the key left of @1@ is
-- @grave@ or @sub@ depending on layout, that shift makes @a@ into @A@, that
-- compose then @\'@ then @e@ makes @é@, that a Chinese input method is running
-- at all -- lives in the keymap the compositor sends as a file descriptor, and
-- interpreting it is what libxkbcommon is for.
--
-- This matters more than it sounds.  The alternative this replaces was one
-- @river_xkb_bindings_v1@ binding per keysym, and while that is fine for
-- shortcuts it cannot express typing: no dead keys, no compose sequences, no
-- input method, no key repeat, no switching layout mid-prompt.  Anyone whose
-- language needs more than unmodified ASCII simply could not use a prompt.
--
-- There is no Haskell binding to xkbcommon on Stackage, so these are the six
-- calls a prompt needs, bound directly.  Marked @safe@ rather than @unsafe@:
-- they are not hot -- one per keystroke -- and @xkb_keymap_new_from_string@
-- parses a keymap that can run to tens of kilobytes.
module XMonad.River.Xkb
  ( XkbState
  , newXkbState
  , freeXkbState
  , keycodeToKeysym
  , keycodeToUtf8
  , updateModifiers
  , modifierActive
  , defaultKeymapText
  , compileKeymap
  , keymapLayoutNames
  ) where

import Control.Monad (when)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.ForeignPtr ()
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (poke, sizeOf)

data XkbContext
data XkbKeymap
data XkbStateT

-- | A parsed keymap and the modifier state that goes with it.
data XkbState = XkbState
  { xkbCtx    :: !(Ptr XkbContext)
  , xkbKeymap :: !(Ptr XkbKeymap)
  , xkbState  :: !(Ptr XkbStateT)
  }

foreign import ccall safe "xkb_context_new"
  c_context_new :: CInt -> IO (Ptr XkbContext)
foreign import ccall safe "xkb_context_unref"
  c_context_unref :: Ptr XkbContext -> IO ()
foreign import ccall safe "xkb_keymap_new_from_string"
  c_keymap_new :: Ptr XkbContext -> CString -> CInt -> CInt -> IO (Ptr XkbKeymap)
foreign import ccall safe "xkb_keymap_unref"
  c_keymap_unref :: Ptr XkbKeymap -> IO ()
foreign import ccall safe "xkb_keymap_new_from_names"
  c_keymap_new_from_names :: Ptr XkbContext -> Ptr () -> CInt -> IO (Ptr XkbKeymap)
foreign import ccall safe "xkb_keymap_get_as_string"
  c_keymap_get_as_string :: Ptr XkbKeymap -> CInt -> IO CString
foreign import ccall safe "xkb_keymap_num_layouts"
  c_keymap_num_layouts :: Ptr XkbKeymap -> IO Word32
foreign import ccall safe "xkb_keymap_layout_get_name"
  c_keymap_layout_get_name :: Ptr XkbKeymap -> Word32 -> IO CString
foreign import ccall safe "xkb_state_new"
  c_state_new :: Ptr XkbKeymap -> IO (Ptr XkbStateT)
foreign import ccall safe "xkb_state_unref"
  c_state_unref :: Ptr XkbStateT -> IO ()
foreign import ccall safe "xkb_state_key_get_one_sym"
  c_get_one_sym :: Ptr XkbStateT -> Word32 -> IO Word32
foreign import ccall safe "xkb_state_key_get_utf8"
  c_get_utf8 :: Ptr XkbStateT -> Word32 -> Ptr CChar -> CSize -> IO CInt
foreign import ccall safe "xkb_state_update_mask"
  c_update_mask :: Ptr XkbStateT -> Word32 -> Word32 -> Word32
                -> Word32 -> Word32 -> Word32 -> IO CInt
foreign import ccall safe "xkb_state_mod_name_is_active"
  c_mod_name_is_active :: Ptr XkbStateT -> CString -> CInt -> IO CInt

-- | Parse the keymap the compositor sent.
--
-- The text is what was read from the @wl_keyboard.keymap@ descriptor.  Returns
-- 'Nothing' if it will not parse, which should not happen and is not worth
-- crashing over: a prompt that cannot read the keyboard is better than a
-- window manager that exits.
newXkbState :: String -> IO (Maybe XkbState)
newXkbState keymapText = do
  ctx <- c_context_new 0
  if ctx == nullPtr then pure Nothing else do
    km <- withCString keymapText $ \s ->
      -- 1 is XKB_KEYMAP_FORMAT_TEXT_V1; 0 is no compile flags.
      c_keymap_new ctx s 1 0
    if km == nullPtr
      then c_context_unref ctx >> pure Nothing
      else do
        st <- c_state_new km
        if st == nullPtr
          then c_keymap_unref km >> c_context_unref ctx >> pure Nothing
          else pure (Just (XkbState ctx km st))

freeXkbState :: XkbState -> IO ()
freeXkbState x = do
  c_state_unref (xkbState x)
  c_keymap_unref (xkbKeymap x)
  c_context_unref (xkbCtx x)

-- | The keysym a keycode currently produces.
--
-- Wayland reports evdev keycodes and xkb expects X11 ones, which differ by a
-- constant 8.  Getting that wrong shifts every key by roughly one row, which
-- looks like a broken layout rather than an off-by-eight.
keycodeToKeysym :: XkbState -> Word32 -> IO Word32
keycodeToKeysym x code = c_get_one_sym (xkbState x) (code + 8)

-- | The text a keycode currently produces, which is not the same question.
--
-- A keysym identifies a key's meaning; this is what should appear in a text
-- field, and the two diverge exactly where it matters -- dead keys and compose
-- sequences produce no text until the sequence completes, and one keystroke
-- can produce several bytes.
keycodeToUtf8 :: XkbState -> Word32 -> IO String
keycodeToUtf8 x code = allocaBytes 64 $ \buf -> do
  n <- c_get_utf8 (xkbState x) (code + 8) buf 64
  if n <= 0 then pure "" else peekCString buf

-- | Track the modifier state the compositor reports.
--
-- Without this the keymap is consulted as though nothing were held, so shift
-- would never capitalise and a layout's third level would be unreachable.
updateModifiers :: XkbState -> Word32 -> Word32 -> Word32 -> Word32 -> IO ()
updateModifiers x depressed latched locked group = do
  r <- c_update_mask (xkbState x) depressed latched locked 0 0 group
  when (r == 0) (pure ())

-- | Whether a named modifier is currently held.
--
-- By name, not by bit position: a modifier's index is a property of the keymap
-- being used, so the bit that means @Mod4@ under one layout can mean something
-- else under another.  The names are xkb's canonical ones -- @\"Shift\"@,
-- @\"Control\"@, @\"Mod1\"@ through @\"Mod5\"@, @\"Lock\"@ -- and are what the
-- @XKB_MOD_NAME_*@ macros expand to.
--
-- 'False' for a name the keymap does not define, which is the same answer as
-- "defined but not held" and is the right one for both: a caller asking
-- whether @Mod3@ is down on a keymap without a @Mod3@ wants @no@, not an
-- error.  libxkbcommon distinguishes them by returning -1, which is folded in
-- here.
--
-- The state consulted is the /effective/ one -- depressed, latched and locked
-- combined -- because that is what decides which keysym the key produced, and
-- a binding is matched against the same thing a person sees on screen.
modifierActive :: XkbState -> String -> IO Bool
modifierActive x name = withCString name $ \cs -> do
  -- 8 is XKB_STATE_MODS_EFFECTIVE.
  r <- c_mod_name_is_active (xkbState x) cs 8
  pure (r > 0)

-- | The compositor's default keymap, as text.
--
-- Passing a null @xkb_rule_names@ asks libxkbcommon for whatever the
-- environment says the default layout is, which is what every other client
-- would get.  Exists for the tests: a headless seat has no keyboard, so a spec
-- that wants to press a key has to give the seat one through
-- @virtual-keyboard-unstable-v1@, and a virtual keyboard has to supply the
-- keymap itself.  Writing one by hand would be a second, worse source of truth
-- for what @a@ or @Super@ means.
--
-- 'Nothing' if libxkbcommon cannot produce one, which would mean no xkb data
-- is installed.
-- | A keymap compiled from RMLVO names -- rules, model, layouts, variants,
-- options, comma-separated as xkb takes them, empty for its default -- as
-- @XKB_KEYMAP_FORMAT_TEXT_V1@ text.  'Nothing' if xkb cannot compile it.
compileKeymap :: String -> String -> String -> String -> String -> IO (Maybe String)
compileKeymap rules model layouts variants options =
  withKeymap rules model layouts variants options $ \km -> do
    cs <- c_keymap_get_as_string km 1
    if cs == nullPtr then pure Nothing else Just <$> peekCString cs

-- | The layouts' names, in index order: what @xkb_keymap_layout_get_name@
-- answers, e.g. @English (US)@.
keymapLayoutNames :: String -> String -> String -> String -> String -> IO (Maybe [String])
keymapLayoutNames rules model layouts variants options =
  withKeymap rules model layouts variants options $ \km -> do
    n <- c_keymap_num_layouts km
    Just <$> mapM (\i -> c_keymap_layout_get_name km i >>= \cs ->
                     if cs == nullPtr then pure "" else peekCString cs) [0 .. n - 1]

-- | A keymap compiled from RMLVO, for the duration of the action; 'Nothing'
-- if xkb cannot compile it.
withKeymap :: String -> String -> String -> String -> String -> (Ptr XkbKeymap -> IO (Maybe a)) -> IO (Maybe a)
withKeymap rules model layouts variants options k =
  withField rules $ \pr -> withField model $ \pm -> withField layouts $ \pl ->
  withField variants $ \pv -> withField options $ \po ->
  allocaBytes (5 * sizeOf pr) $ \names -> do
    mapM_ (\(i, p) -> poke (names `plusPtr` (i * sizeOf pr)) p) (zip [0 ..] [pr, pm, pl, pv, po])
    ctx <- c_context_new 0
    if ctx == nullPtr then pure Nothing else do
      km <- c_keymap_new_from_names ctx names 0
      if km == nullPtr
        then c_context_unref ctx >> pure Nothing
        else do
          out <- k km
          c_keymap_unref km
          c_context_unref ctx
          pure out
  where
    withField "" k' = k' nullPtr
    withField s k' = withCString s k'

defaultKeymapText :: IO (Maybe String)
defaultKeymapText = do
  ctx <- c_context_new 0
  if ctx == nullPtr then pure Nothing else do
    km <- c_keymap_new_from_names ctx nullPtr 0
    if km == nullPtr
      then c_context_unref ctx >> pure Nothing
      else do
        cs <- c_keymap_get_as_string km 1  -- XKB_KEYMAP_FORMAT_TEXT_V1
        out <- if cs == nullPtr then pure Nothing else Just <$> peekCString cs
        c_keymap_unref km
        c_context_unref ctx
        pure out
