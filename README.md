haskell-mpv
===========

A thread-safe, mid-level Haskell wrapper around [libmpv](https://mpv.io/).


Usage
-----

A very spartan media player in 18 lines, using `Codec.MPV.Simple`:

```haskell
module Main where
import Codec.MPV.Simple
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import System.Environment (getArgs)

main :: IO ()
main = do
  files <- getArgs
  fileDone <- newEmptyMVar
  mpv <- simpleMPV simpleSettings { eventHandler = handleEOF fileDone }
  mapM_ (playFile mpv fileDone) files

handleEOF :: MVar () -> MPV -> MPVEvent -> IO ()
handleEOF fileDone mpv (EndFileEvent _) = putMVar fileDone ()
handleEOF fileDone mpv _                = return ()

playFile :: MPV -> MVar () -> FilePath -> IO ()
playFile mpv fileDone file = loadFile mpv file >> takeMVar fileDone
```

For more control over rendering, window management, and the MPV event loop,
use `Codec.MPV` instead.


Troubleshooting
---------------

### I don't get any video output!

* Did you create an OpenGL rendering context and set it as the current context
  (`glMakeCurrent`)?
* Are you calling `renderFrame` in response to `RenderEvent`s?
* Are you properly presenting your rendering buffer after calling `renderFrame`
  (`glSwapBuffers`)?
