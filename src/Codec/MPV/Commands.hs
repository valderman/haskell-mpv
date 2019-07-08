-- | Higher-level commands build on top of 'sendCommand'.
module Codec.MPV.Commands where
import Codec.MPV.Core
import Codec.MPV.Types (MPV)

data SeekMode = Absolute | Relative
  deriving (Show, Read, Eq)

data ScreenshotMode = WithSubs | WithoutSubs

-- | Load and play the given file.
loadFile :: MPV -> FilePath -> IO ()
loadFile mpv f = sendCommand mpv ["loadfile", f]

-- | Load the given subtitle file and set it as the active subtitle track
--   for the currently playing video.
loadSubtitle :: MPV -> FilePath -> IO ()
loadSubtitle mpv f = sendCommand mpv ["sub-add", f]

-- | Show the given text on the OSD.
showOSD :: MPV -> String -> IO ()
showOSD mpv s = sendCommand mpv ["show-text", s]

-- | Show progressbar, elapsed time and total time on the OSD.
showProgress :: MPV -> IO ()
showProgress = flip sendCommand ["show-progress"]

-- | Stop and unload the currently playing video.
stopPlayback :: MPV -> IO ()
stopPlayback = flip sendCommand ["stop"]

-- | Pause playback of the current video.
pausePlayback :: MPV -> IO ()
pausePlayback = flip sendCommand ["set_property", "pause", "true"]

-- | Resume playback of the current video.
resumePlayback :: MPV -> IO ()
resumePlayback = flip sendCommand ["set_property", "pause", "false"]

-- | Pause the current video if playing, resume playback if paused.
playPause :: MPV -> IO ()
playPause = flip sendCommand ["cycle", "pause"]

-- | Seek to the given time, either as an offset from the current position
--   if mode is 'Relative', or to the exact time if mode is 'Absolute'.
--   The given time may be negative.
seek :: MPV -> SeekMode -> Int -> IO ()
seek mpv mode t = sendCommand mpv ["seek", show t, showMode mode]
  where
    showMode Absolute = "absolute"
    showMode Relative = "relative"

-- | Save a screenshot to the given file.
screenshot :: MPV -> ScreenshotMode -> FilePath -> IO ()
screenshot mpv mode f = sendCommand mpv ["screenshot-to-file", f, showMode mode]
  where
    showMode WithSubs    = "subtitles"
    showMode WithoutSubs = "videp"
