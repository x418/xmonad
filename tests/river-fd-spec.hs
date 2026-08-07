-- | File descriptor passing over a Wayland-shaped socket.
--
-- The property that matters is not "an fd can be sent" but "an fd rides on the
-- same message as the request bytes, on a *connected* socket".  Wayland
-- correlates descriptors with requests by order, so a scheme that sends them
-- separately -- as @network@'s @sendFd@ does -- is no use, and @network@'s
-- @sendMsg@ cannot be used at all here because it always sets @msg_name@ and
-- Linux answers @EISCONN@.
--
-- Without this working there is no @wl_shm_pool@, so no @wl_buffer@, so no way
-- for the window manager to draw anything at all.
module Main (main) where

import Control.Monad (unless, forM_)
import System.Exit (exitFailure)
import System.Posix.IO (closeFd, createPipe, fdRead, fdWrite)
import System.Posix.Types (Fd)
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket as N

import XMonad.River.Socket (recvWithFds, sendWithFds)

main :: IO ()
main = do
  results <- sequence
    [ payloadAndFdArriveTogether
    , severalFdsInOneMessage
    , noFdsIsAnOrdinaryWrite
    , fdsArriveInOrder
    ]
  forM_ results $ \(name, ok) ->
    putStrLn ((if ok then "PASS " else "FAIL ") ++ name)
  unless (all snd results) exitFailure

-- | A connected AF_UNIX stream pair, which is what a Wayland connection is.
withPair :: (N.Socket -> N.Socket -> IO a) -> IO a
withPair k = do
  (a, b) <- N.socketPair N.AF_UNIX N.Stream N.defaultProtocol
  r <- k a b
  N.close a >> N.close b
  pure r

-- | A pipe whose read end carries something recognisable.
withMarkedFd :: String -> (Fd -> IO a) -> IO a
withMarkedFd marker k = do
  (r, w) <- createPipe
  _ <- fdWrite w marker
  closeFd w
  a <- k r
  closeFd r
  pure a

readMarker :: Fd -> IO String
readMarker fd = fst <$> fdRead fd 64

payloadAndFdArriveTogether :: IO (String, Bool)
payloadAndFdArriveTogether = fmap ((,) "payload and descriptor arrive together") $
  withPair $ \a b -> withMarkedFd "pool" $ \fd -> do
    _ <- sendWithFds a (BC.pack "REQUEST") [fd]
    (bs, fds) <- recvWithFds b 64
    case fds of
      [got] -> do
        marker <- readMarker got
        closeFd got
        pure (bs == BC.pack "REQUEST" && marker == "pool")
      _ -> pure False

severalFdsInOneMessage :: IO (String, Bool)
severalFdsInOneMessage = fmap ((,) "several descriptors in one message") $
  withPair $ \a b -> withMarkedFd "one" $ \f1 -> withMarkedFd "two" $ \f2 -> do
    _ <- sendWithFds a (BC.pack "TWO") [f1, f2]
    (_, fds) <- recvWithFds b 64
    markers <- mapM readMarker fds
    mapM_ closeFd fds
    pure (markers == ["one", "two"])

-- | The common case: almost every Wayland request carries no descriptor, and
-- must still go out as a plain write.
noFdsIsAnOrdinaryWrite :: IO (String, Bool)
noFdsIsAnOrdinaryWrite = fmap ((,) "a message with no descriptors still sends") $
  withPair $ \a b -> do
    n <- sendWithFds a (BC.pack "PLAIN") []
    (bs, fds) <- recvWithFds b 64
    pure (n == 5 && bs == BC.pack "PLAIN" && null fds)

-- | Wayland matches descriptors to requests positionally, so order is part of
-- the contract rather than an implementation detail.
fdsArriveInOrder :: IO (String, Bool)
fdsArriveInOrder = fmap ((,) "descriptors keep their order") $
  withPair $ \a b ->
    withMarkedFd "a" $ \f1 -> withMarkedFd "b" $ \f2 -> withMarkedFd "c" $ \f3 -> do
      _ <- sendWithFds a (BC.pack "ORDER") [f1, f2, f3]
      (_, fds) <- recvWithFds b 64
      markers <- mapM readMarker fds
      mapM_ closeFd fds
      pure (markers == ["a", "b", "c"])
