-- | C level API for MPV.
{-# LANGUAGE ForeignFunctionInterface #-}
module Codec.MPV.CAPI where
import Data.Int (Int64)
import Foreign.C
import Foreign.Ptr

data MPVHandle
data MPVEventHandle
data MPVCtx
type Callback = Ptr () -> IO ()

pTR_SIZE = 8 :: Int

mPV_EVENT_NONE = 0 :: CInt
mPV_EVENT_SHUTDOWN = 1 :: CInt
mPV_EVENT_LOG_MESSAGE = 2 :: CInt
mPV_EVENT_START_FILE = 6 :: CInt
mPV_EVENT_END_FILE = 7 :: CInt
mPV_EVENT_FILE_LOADED = 8 :: CInt
mPV_EVENT_IDLE = 11 :: CInt
mPV_EVENT_TICK = 14 :: CInt
mPV_EVENT_VIDEO_RECONFIG = 17 :: CInt
mPV_EVENT_AUDIO_RECONFIG = 18 :: CInt
mPV_EVENT_SEEK = 20 :: CInt
mPV_EVENT_PLAYBACK_RESTART = 21 :: CInt
mPV_EVENT_PROPERTY_CHANGE = 22 :: CInt
mPV_EVENT_QUEUE_OVERFLOW = 24 :: CInt

foreign import ccall "mpv_create"
  mpv_create :: IO (Ptr MPVHandle)
foreign import ccall "mpv_initialize"
  mpv_initialize :: Ptr MPVHandle -> IO CInt
foreign import ccall "mpv_terminate_destroy"
  mpv_terminate_destroy :: Ptr MPVHandle -> IO ()
foreign import ccall "mpv_wait_event"
  mpv_wait_event :: Ptr MPVHandle -> CDouble -> IO (Ptr MPVEventHandle)
foreign import ccall "mpv_command"
  mpv_command :: Ptr MPVHandle -> CString -> IO CInt
foreign import ccall "mpv_error_string"
  mpv_error_string :: CInt -> IO CString
foreign import ccall "mpv_render_context_free"
  mpv_render_context_free :: Ptr MPVCtx -> IO ()
foreign import ccall "mpv_set_wakeup_callback"
  mpv_set_wakeup_callback :: Ptr MPVHandle -> FunPtr Callback -> Ptr () -> IO ()
foreign import ccall "mpv_render_context_set_update_callback"
  mpv_render_context_set_update_callback
    :: Ptr MPVCtx
    -> FunPtr Callback
    -> Ptr ()
    -> IO ()
foreign import ccall "mpv_get_property_string"
  mpv_get_property_string :: Ptr MPVHandle -> CString -> IO CString
foreign import ccall "mpv_set_property_string"
  mpv_set_property_string :: Ptr MPVHandle -> CString -> CString -> IO CInt
foreign import ccall "mpv_observe_property"
  mpv_observe_property :: Ptr MPVHandle -> Int64 -> CString -> CInt -> IO CInt
foreign import ccall "mpv_unobserve_property"
  mpv_unobserve_property :: Ptr MPVHandle -> Int64 -> IO CInt
foreign import ccall "mpv_free"
  mpv_free :: CString -> IO ()
foreign import ccall "mpv_request_log_messages"
  mpv_request_log_messages :: Ptr MPVHandle -> CString -> IO CInt
foreign import ccall "mpv_set_option_string"
  mpv_set_option_string :: Ptr MPVHandle -> CString -> CString -> IO CInt

foreign import ccall "create_mpv_gl_ctx"
  create_mpv_gl_ctx :: Ptr MPVHandle -> IO (Ptr MPVCtx)
foreign import ccall "render_to_default_fbo"
  render_to_default_fbo :: Ptr MPVCtx -> CInt -> CInt -> IO ()
foreign import ccall "wrapper"
  mkCallback :: Callback -> IO (FunPtr Callback)
