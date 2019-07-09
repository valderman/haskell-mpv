{-# LANGUAGE OverloadedStrings #-}
module Codec.MPV.Types where
import Control.Concurrent (MVar)
import Control.Exception
import Data.Int (Int64)
import Data.IORef (IORef)
import Data.Text (Text, unpack)
import Foreign.Ptr (Ptr, FunPtr)
import Codec.MPV.CAPI
import Codec.MPV.Chan (Chan)

-- | The name of some MPV property.
type PropertyName = Text

-- | The name of some MPV option.
type OptionName = Text

-- | A handle to a watch initiated by 'watchProperty'.
--   Can be passed to 'unwatchProperty' to stop receiving new updates for
--   that watch.
newtype WatchHandle = W { unW :: Int64 }
  deriving (Eq, Ord, Show)

-- | An internal MPV event: either an actual MPV event, or a notification for
--   the main thread that there are new events to fetch in the event queue.
data MPVInternalEvent = Event MPVEvent | Wakeup

-- | A handle to an MPV instance.
data MPV = MPV
  { mpvHandle :: MVar (Maybe (Ptr MPVHandle))
  , mpvGLCtx :: Ptr MPVCtx
  , mpvEventChan :: Chan MPVInternalEvent
  , mpvOnEvent :: FunPtr Callback
  , mpvOnRender :: FunPtr Callback
  , mpvFinalized :: IORef Bool
  }

-- | Exception signalling that an MPV error occurred.
data MPVErrorException = MPVErrorException
  { mpvErrorCode :: Int
  , mpvErrorDescription :: Text
  }

instance Show MPVErrorException where
  show e = concat
    [ "MPV error ", show (mpvErrorCode e), ": "
    , unpack (mpvErrorDescription e)
    ]
instance Exception MPVErrorException

-- | Exception signalling that no SDL/OpenGL context is available.
data MPVNoSDLGLException = MPVNoSDLGLException
  deriving Show
instance Exception MPVNoSDLGLException

-- | Exception signalling that an unsupported (i.e. non-string) property format
--   was encountered. The argument gives the @mpv_format@ format ID.
data MPVFormatException = MPVFormatException Int
  deriving Show
instance Exception MPVFormatException

data MPVAlreadyDestroyedException = MPVAlreadyDestroyedException
  deriving Show
instance Exception MPVAlreadyDestroyedException

data MPVLogLevel
  = LogNone
  | LogFatal
  | LogError
  | LogWarn
  | LogInfo
  | LogVerbose
  | LogDebug
  | LogTrace
    deriving (Show, Read, Eq)

logLevelStr :: MPVLogLevel -> Text
logLevelStr LogNone    = "no"
logLevelStr LogFatal   = "fatal"
logLevelStr LogError   = "error"
logLevelStr LogWarn    = "warn"
logLevelStr LogInfo    = "info"
logLevelStr LogVerbose = "verbose"
logLevelStr LogDebug   = "debug"
logLevelStr LogTrace   = "trace"

data MPVEndFileReason
  = EndFileEOF
  | EndFileStop
  | EndFileQuit
  | EndFileRedirect
    deriving (Show, Read, Eq)

data MPVEvent
  = ShutdownEvent
  | LogEvent Text MPVLogLevel Text
  | StartFileEvent
  | EndFileEvent MPVEndFileReason
  | FileLoadedEvent
  | IdleEvent
  | TickEvent
  | VideoReconfigEvent
  | AudioReconfigEvent
  | SeekEvent
  | PlaybackRestartEvent
  | PropertyChangeEvent Text Text
  | EventQueueOverflowEvent
  | RenderEvent
    deriving (Show, Eq)
