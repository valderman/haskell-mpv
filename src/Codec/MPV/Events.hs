-- | MPV event handling.
module Codec.MPV.Events (waitEvent) where
import Control.Concurrent hiding (Chan)
import Control.Exception
import Control.Monad
import Foreign.C
import Foreign.Ptr (nullPtr)
import Foreign.Storable
import Codec.MPV.CAPI
import Codec.MPV.Chan
import Codec.MPV.Core (check, destroyFinal)
import Codec.MPV.Types

-- | Return the next event in the MPV event queue.
--   Blocks until an event arrives, if the queue is empty.
--   Implementors must respond to the 'RenderEvent' by calling
--   'mpvRenderFrame'; handling any other events is optional.
--
--   Will always return 'ShutdownEvent' if called on an already destroyed
--   MPV instance.
waitEvent :: MPV -> IO MPVEvent
waitEvent mpv = do
    evt <- getChan (mpvEventChan mpv)
    case evt of
      Just Wakeup    -> readAllEvents >> waitEvent mpv
      Just (Event e) -> return e
      Nothing        -> return ShutdownEvent
  where
    readAllEvents = do
      more <- modifyMVar (mpvHandle mpv) $ \mh ->
        case mh of
          Just h -> do
            evt <- mpv_wait_event h 0
            if evt == nullPtr
              then return (mh, False)
              else readNextEvent mh evt
          _ -> do
            closeChan (mpvEventChan mpv)
            return (Nothing, False)
      when more $ readAllEvents

    readNextEvent mh evt = do
      eid <- peekByteOff evt 0
      case () of
        _ | eid == mPV_EVENT_NONE -> do
              return (mh, False)
          | eid == mPV_EVENT_SHUTDOWN -> do
              destroyFinal mpv
              closeChan (mpvEventChan mpv)
              return (Nothing, False)
          | otherwise -> do
              me <- readEvent evt
              maybe (pure ()) (putChan (mpvEventChan mpv) . Event) me
              return (mh, True)

    getEventData evt = peekByteOff evt (pTR_SIZE + 8)

    getLogEvent evt = do
      evtdata <- getEventData evt
      prefix <- peekCString =<< peekByteOff evtdata 0
      msg <- peekCString =<< peekByteOff evtdata (pTR_SIZE*2)
      levelNum <- peekByteOff evtdata (pTR_SIZE*3)
      let level = case levelNum :: CInt of
            0  -> LogNone
            10 -> LogFatal
            20 -> LogError
            30 -> LogWarn
            40 -> LogInfo
            50 -> LogVerbose
            60 -> LogDebug
            70 -> LogTrace
      return $ LogEvent prefix level msg

    getEndFileEvent evt = do
      evtdata <- getEventData evt
      reasonNum <- peekByteOff evtdata 0
      reason <- case reasonNum :: CInt of
            0 -> pure EndFileEOF
            2 -> pure EndFileStop
            3 -> pure EndFileQuit
            4 -> check (peekByteOff evtdata 4) >> error "unrechable"
            5 -> pure EndFileRedirect
      return $ EndFileEvent reason

    getPropertyChangeEvent evt = do
      evtdata <- getEventData evt
      name <- peekCString =<< peekByteOff evtdata 0
      format <- peekByteOff evtdata pTR_SIZE
      when ((format :: CInt) /= 1) $ do
        throw $ MPVFormatException (fromIntegral format)
      textptr <- peekByteOff evtdata (pTR_SIZE*2)
      ctext <- peekByteOff textptr 0
      text <- peekCString ctext
      return $ PropertyChangeEvent name text

    readEvent evt = do
      eid <- peekByteOff evt 0
      case eid :: CInt of
        _ | eid == mPV_EVENT_LOG_MESSAGE -> do
              Just <$> getLogEvent evt
          | eid == mPV_EVENT_START_FILE -> do
              return $ Just StartFileEvent
          | eid == mPV_EVENT_END_FILE -> do
              Just <$> getEndFileEvent evt
          | eid == mPV_EVENT_FILE_LOADED -> do
              return $ Just FileLoadedEvent
          | eid == mPV_EVENT_IDLE -> do
              return $ Just IdleEvent
          | eid == mPV_EVENT_TICK -> do
              return $ Just TickEvent
          | eid == mPV_EVENT_VIDEO_RECONFIG -> do
              return $ Just VideoReconfigEvent
          | eid == mPV_EVENT_AUDIO_RECONFIG -> do
              return $ Just AudioReconfigEvent
          | eid == mPV_EVENT_SEEK -> do
              return $ Just SeekEvent
          | eid == mPV_EVENT_PLAYBACK_RESTART -> do
              return $ Just PlaybackRestartEvent
          | eid == mPV_EVENT_PROPERTY_CHANGE -> do
              Just <$> getPropertyChangeEvent evt
          | eid == mPV_EVENT_QUEUE_OVERFLOW -> do
              return $ Just EventQueueOverflowEvent
          | otherwise -> do
              pure Nothing
