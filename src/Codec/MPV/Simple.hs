-- | A simplified interface which uses SDL to create a window and an OpenGL
--   rendering context, and automatically render each played frame to that
--   window.
module Codec.MPV.Simple
  ( module Codec.MPV
  , SimpleSettings (..), WindowMode (..), MPV.MPVEvent (..)
  , simpleMPV, simpleSettings
  ) where
import Control.Concurrent
import Control.Exception (finally)
import qualified Data.Text as Text (pack)
import qualified SDL
import qualified SDL.Video as SDL
import qualified SDL.Video.OpenGL as SDL
import Codec.MPV hiding (waitEvent, create, destroy, withMPV, renderFrame)
import Codec.MPV as MPV

data WindowMode
  = Full
  | FullDesktop
  | Window Int Int

-- | Settings for 'simpleMPV'.
data SimpleSettings = SimpleSettings
  { -- | Title for the video playback window.
    windowTitle :: String

    -- | How should the window be displayed?
  , windowMode :: WindowMode

    -- | Function to call when a new MPV event arrives.
    --   When a 'RenderEvent' arrives, this function will be called after the
    --   video frame is rendered to the current OpenGL context but before the
    --   current buffer is displayed to the user, to allow the handler to
    --   perform any additional rendering on top of the frame.
  , eventHandler :: MPV.MPV -> MPV.MPVEvent -> IO ()
  }

-- | Default settings for @simpleMPV@.
simpleSettings :: SimpleSettings
simpleSettings = SimpleSettings
  { windowTitle = "simpleMPV"
  , windowMode = FullDesktop
  , eventHandler = \_ _ -> pure ()
  }

-- | Create an MPV instance with associated window to which its video output
--   will be rendered as soon as a new frame is available.
--
--   This function creates and maintains its MPV instance on a separate thread.
--   Thus, it is never safe to call 'MPV.destroy' on the returned @MPV@ handle.
--   Use 'quit' instead.
simpleMPV :: SimpleSettings -> IO MPV.MPV
simpleMPV settings = do
    mpvHandleVar <- newEmptyMVar
    forkOS $ do
      (win, ctx) <- initSDL
      SDL.V2 w h <- SDL.glGetDrawableSize win
      mpv <- MPV.create
      putMVar mpvHandleVar mpv

      let handler = eventHandler settings
          w' = fromIntegral w
          h' = fromIntegral h
      eventLoop mpv win handler w' h' `finally` cleanup mpv win ctx
    takeMVar mpvHandleVar
  where
    eventLoop mpv win handler w h = go
      where
        go = do
          evt <- MPV.waitEvent mpv
          case evt of
            MPV.RenderEvent -> do
              MPV.renderFrame mpv w h
              handler mpv evt
              SDL.glSwapWindow win
              go
            MPV.ShutdownEvent -> do
              handler mpv evt
            _ -> do
              forkIO (handler mpv evt) >> go

    cleanup mpv win ctx = do
      MPV.destroy mpv
      SDL.glDeleteContext ctx
      SDL.destroyWindow win

    windowSettings = SDL.defaultWindow
      { SDL.windowMode = case windowMode settings of
          Full        -> SDL.Fullscreen
          FullDesktop -> SDL.FullscreenDesktop
          Window w h  -> SDL.Windowed
      , SDL.windowInitialSize = case windowMode settings of
          Window w h -> SDL.V2 (fromIntegral w) (fromIntegral h)
          _          -> SDL.V2 0 0
      , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
      }

    initSDL = do
      SDL.initialize [SDL.InitVideo]
      w <- SDL.createWindow (Text.pack $ windowTitle settings) windowSettings
      c <- SDL.glCreateContext w
      SDL.glMakeCurrent w c
      return (w, c)
