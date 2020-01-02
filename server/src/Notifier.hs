module Notifier (Notifier, new, await, ping) where
import Control.Concurrent
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef

data Notifier a = Notifier
  { vars :: IORef [MVar a]
  }

new :: MonadIO m => m (Notifier a)
new = liftIO $ Notifier <$> newIORef []

await :: MonadIO m => Notifier a -> m a
await n = liftIO $ do
  v <- newEmptyMVar
  atomicModifyIORef' (vars n) $ \vs -> (v:vs, ())
  takeMVar v

ping :: MonadIO m => Notifier a -> a -> m ()
ping n msg = liftIO $ do
  vs <- atomicModifyIORef' (vars n) $ \vs -> ([], vs)
  void $ forkIO $ mapM_ (flip putMVar msg) vs
