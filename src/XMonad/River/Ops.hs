-- | One-shot requests from the 'X' monad.
--
-- User code never touches the connection: it queues what it wants and the
-- event loop sends it, inside a manage sequence for 'emitOp' and on the next
-- pass for 'emitNow'.  See "XMonad.River.Plan" for which is which.
module XMonad.River.Ops (emitOp, emitNow) where

import Control.Monad.Reader (asks)

import XMonad.Core (X, XConf(..))
import XMonad.River.Plan (Op)
import XMonad.River.State (queueNow, queueOp)

-- | Ask for a request to go out with the next manage sequence.
emitOp :: Op -> X ()
emitOp op = asks riverState >>= \rs -> queueOp rs op

-- | Ask for a request to go out on the event loop's next pass, sequence or
-- not: ending the session, asking river to release this window manager.
emitNow :: Op -> X ()
emitNow op = asks riverState >>= \rs -> queueNow rs op
