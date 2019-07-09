{-# LANGUAGE OverloadedStrings #-}
module Codec.MPV.Core
  ( MPV, MPVLogLevel (..), MPVEndFileReason (..), MPVEvent (..)
  , PropertyName, OptionName, WatchHandle
  , MPVErrorException (..), MPVNoSDLGLException (..), MPVFormatException (..)
  , MPVAlreadyDestroyedException (..)
  , create, withMPV
  , quit, destroy
  , sendCommand, renderFrame
  , setOption, setLogLevel
  , getProperty, setProperty, watchProperty, unwatchProperty
    -- Only for internal use; do not re-export!
  , check, destroyFinal
  ) where
import Control.Concurrent hiding (Chan, newChan)
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString as BS
import Foreign.C
import Foreign.Marshal hiding (void)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import Codec.MPV.CAPI
import Codec.MPV.Chan
import Codec.MPV.Types

-- | Check whether an MPV operation succeeded.
--   Throws 'MPVErrorException' with an appropriate error code and message
--   on failure.
check :: IO CInt -> IO ()
check m = do
  res <- m
  when (res < 0) $ do
    str <- fmap decodeUtf8 . BS.packCString =<< mpv_error_string res
    throw $ MPVErrorException
      { mpvErrorCode = fromIntegral res
      , mpvErrorDescription = str
      }

withHandle :: MPV -> (Ptr MPVHandle -> IO a) -> IO a
withHandle mpv f = withMVar (mpvHandle mpv) $ \mh -> do
  case mh of
    Just h -> f h
    _      -> throw MPVAlreadyDestroyedException

{-# NOINLINE nextWatchHandle #-}
nextWatchHandle :: IORef WatchHandle
nextWatchHandle = unsafePerformIO $ newIORef (W 0)

freshWatchHandle :: IO WatchHandle
freshWatchHandle = do
  atomicModifyIORef' nextWatchHandle $ \h -> (W (unW h+1), h)

-- | Run the given computation with an MPV handle.
--   The handle can (and must) be used to wait for MPV events periodically,
--   as well as send commands to MPV.
--
--   When the computation finishes, the MPV instance will be terminated and
--   the handle deallocated, so the handle must not be used after this point.
--
--   Has the same precondition requirements as 'create'.
withMPV :: (MPV -> IO a) -> IO a
withMPV m = do
  mpv <- create
  m mpv `finally` destroy mpv

-- | Create a new MPV instance.
--   By default, attempts to enable hardware-accelerated video decoding for
--   supported formats.
--
--   Before calling @init@, you must initialize SDL's video subsystem and
--   set the current OpenGL context, or undefined behavior happens.
create :: IO MPV
create = do
  mpv <- mpv_create
  check $ mpv_initialize mpv
  ctx <- create_mpv_gl_ctx mpv
  when (ctx == nullPtr) $ throw MPVNoSDLGLException

  eventChan <- newChan
  onEvent <- mkCallback $ const $ putChan eventChan Wakeup
  mpv_set_wakeup_callback mpv onEvent nullPtr
  onRender <- mkCallback $ const $ putChan eventChan (Event RenderEvent)
  mpv_render_context_set_update_callback ctx onRender nullPtr

  mpvVar <- newMVar (Just mpv)
  finalized <- newIORef False
  let mpv' = MPV mpvVar ctx eventChan onEvent onRender finalized
  setOption mpv' "hwdec" "auto"
  return mpv'

-- | Shutdown the given MPV instance and free all resources associated with it.
--   This function is idempotent.
--
--   Note that this function must be called from the same OS thread that
--   created the MPV instance. Attempting to call it from another thread
--   is highly likely to segfault your program.
--
--   To cause the destruction of an MPV instance from another thread, see
--   'quit'. In fact, you should always prefer @quit@ over this function
--   whenever possible.
destroy :: MPV -> IO ()
destroy mpv = modifyMVar_ (mpvHandle mpv) $ \mh -> do
  case mh of
    Just h -> do
      mpv_render_context_free (mpvGLCtx mpv)
      mpv_terminate_destroy h
      writeIORef (mpvFinalized mpv) True
      freeHaskellFunPtr (mpvOnRender mpv)
      freeHaskellFunPtr (mpvOnEvent mpv)
    _ -> return ()
  return Nothing

-- | Destroy the parts of the MPV interface that are not automatically destroyed
--   by MPV itself after a call to 'quit'. Only ever call in response to
--   @mPV_EVENT_SHUTDOWN@. This function is idempotent.
--
--   Only for internal use; 'waitEvent' handles this cleanup automatically.
destroyFinal :: MPV -> IO ()
destroyFinal mpv = do
  alreadyFinalized <- atomicModifyIORef' (mpvFinalized mpv) (\s -> (True, s))
  unless alreadyFinalized $ do
    freeHaskellFunPtr (mpvOnRender mpv)
    freeHaskellFunPtr (mpvOnEvent mpv)

-- | Send the @quit@ command to the given MPV instance, initialising a
--   controlled shutdown. When this shutdown is completed, a 'ShutdownEvent'
--   will be delivered via the 'waitEvent' function.
--   This function is idempotent.
--
--   Applications must keep processing events until this event is received.
--   Once received however, using the given MPV handle again will result
--   in undefined behavior.
quit :: MPV -> IO ()
quit mpv = sendCommand mpv ["quit"]

-- | Set the level of severity required for a log message to be delivered
--   through a 'LogEvent' message.
--   This function is idempotent.
setLogLevel :: MPV -> MPVLogLevel -> IO ()
setLogLevel mpv lvl = withHandle mpv $ \h -> do
  BS.useAsCString
    (encodeUtf8 $ logLevelStr lvl)
    (check . mpv_request_log_messages h)

-- | Render the current MPV frame with the given width and height to the
--   default frame buffer object of the current OpenGL context.
--
--   Must be called in response to receiving a 'RenderEvent'.
--   Rendering to other FBOs is currently not supported.
renderFrame :: MPV -> Int -> Int -> IO ()
renderFrame mpv w h = withHandle mpv $ \_ -> do
  render_to_default_fbo (mpvGLCtx mpv) (fromIntegral w) (fromIntegral h)

-- | Send the given command to MPV.
--   For a comprehensive list of available commands, see
--   <https://mpv.io/manual/stable/#command-interface>.
sendCommand :: MPV -> [Text] -> IO ()
sendCommand mpv command = withHandle mpv $ \h -> go 1 [] h command
  where
    go n cmds' h (cmd:cmds) = do
      BS.useAsCString (encodeUtf8 cmd) $ \cmd' -> go (n+1) (cmd':cmds') h cmds
    go n cmds' h [] =
      allocaBytes (pTR_SIZE*n) $ \ptr -> do
        forM_ (zip [0, pTR_SIZE ..] (reverse (nullPtr:cmds'))) $ \(off, str) -> do
          pokeByteOff ptr off str
        check $ mpv_command h ptr

-- | Gets the value of the given property.
getProperty :: MPV -> PropertyName -> IO Text
getProperty mpv prop = withHandle mpv $ \h -> do
  result <- BS.useAsCString (encodeUtf8 prop) $ mpv_get_property_string h
  string <- decodeUtf8 <$> BS.packCString result
  mpv_free result
  return string

-- | Gets the value of the given property.
--   This function is idempotent.
setProperty :: MPV -> PropertyName -> Text -> IO ()
setProperty mpv prop value = withHandle mpv $ \h -> do
  BS.useAsCString (encodeUtf8 prop) $ \prop' -> do
    BS.useAsCString (encodeUtf8 value) $ \value' -> do
      check $ mpv_set_property_string h prop' value'

-- | Watch the given property. Whenever the property changes, its new value
--   will be returned from 'waitEvent' as a 'PropertyChangedEvent'.
watchProperty :: MPV -> PropertyName -> IO WatchHandle
watchProperty mpv prop = withHandle mpv $ \h -> do
  BS.useAsCString (encodeUtf8 prop) $ \prop' -> do
    wh <- freshWatchHandle
    check $ mpv_observe_property h (unW wh) prop' 1
    return wh

-- | Stop receiving updates for the given watch.
unwatchProperty :: MPV -> WatchHandle -> IO ()
unwatchProperty mpv wh = withHandle mpv $ \h -> do
  check $ mpv_unobserve_property h (unW wh)

-- | Set the value of the given MPV option.
--   See <https://mpv.io/manual/master/#options> for a complete list of
--   available options.
--   This function is idempotent.
setOption :: MPV -> OptionName -> Text -> IO ()
setOption mpv opt value = withHandle mpv $ \h -> do
  BS.useAsCString (encodeUtf8 opt) $ \opt' -> do
    BS.useAsCString (encodeUtf8 value) $ \value' -> do
      check $ mpv_set_option_string h opt' value'
