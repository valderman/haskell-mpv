module Codec.MPV.Chan (Chan, newChan, getChan, putChan, closeChan) where
import Control.Concurrent hiding (Chan, newChan, readChan, writeChan)
import Control.Exception (Exception, throw)
import Control.Monad (unless, join)

data ChanNotWritableException = ChanNotWritableException
  deriving Show
instance Exception ChanNotWritableException

-- | An amortized, non-empty, constant time queue.
data Q a = Q [a] [a]

readQ :: Q a -> (Either (Q a) [MVar (Maybe a)], a)
readQ (Q [x] [])    = (Right [], x)
readQ (Q (x:xs) ys) = (Left (Q xs ys), x)
readQ (Q [] ys)     = readQ (Q (reverse ys) [])

writeQ :: Q a -> a -> Q a
writeQ (Q xs ys) x = Q xs (x:ys)

nullQ :: Q a -> Bool
nullQ (Q [] []) = True
nullQ _         = False

newQ :: a -> Q a
newQ x = Q [x] []

-- | Internal state of a closable channel.
data ChanState a = CS
  { -- | Either a non-empty queue or a (possibly empty) list of readers waiting
    --   for a value to be written.
    queueOrReaders :: Either (Q a) [MVar (Maybe a)]

    -- | Is the queue open for writing?
  , writable :: Bool
  }

-- | An unbounded, closable channel.
--   Reads from this channel are fair, i.e. if there are multiple readers
--   they are served on a first-come, first-serve basis.
newtype Chan a = C { unC :: MVar (ChanState a) }

-- | Create a new, empty channel.
newChan :: IO (Chan a)
newChan = C <$> newMVar (CS (Right []) True)

-- | Write the given value to the given channel.
--   Throws @ChanNotWritableException@ if the channel is no longer writable.
putChan :: Chan a -> a -> IO ()
putChan (C ch) x = modifyMVar_ ch $ \cs -> do
    unless (writable cs) $ throw ChanNotWritableException
    qor <- put (queueOrReaders cs) x
    return cs { queueOrReaders = qor }
  where
    put (Left q) x       = return (Left (writeQ q x))
    put (Right []) x     = return (Left (newQ x))
    put (Right (v:vs)) x = putMVar v (Just x) >> return (Right vs)

-- | Get a value from the given channel. Blocks until a value is available
--   if the channel is empty. Returns @Nothing@ if the channel is
--   empty and becomes closed for writing before another value can be written
--   to it.
getChan :: Chan a -> IO (Maybe a)
getChan (C ch) = join $ modifyMVar ch $ \cs ->
  case queueOrReaders cs of
    Left q -> do
      let (qor, x) = readQ q
      return (cs { queueOrReaders = qor }, return (Just x))
    Right rs
      | writable cs -> do
          v <- newEmptyMVar
          return (cs { queueOrReaders = Right (rs ++ [v]) }, takeMVar v)
      | otherwise -> do
          return (cs, return Nothing)

-- | Close the given channel for writing.
closeChan :: Chan a -> IO ()
closeChan (C ch) = modifyMVar_ ch $ \cs -> do
  case queueOrReaders cs of
    Right vs -> mapM_ (flip putMVar Nothing) vs >> return (CS (Right []) False)
    _        -> return (cs { writable = False })
