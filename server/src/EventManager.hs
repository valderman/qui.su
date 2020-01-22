-- | Manages per-quiz event notifications.
module EventManager
  ( EventManager
  , Event (..)
  , new, await, notify
  ) where
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.IntMap as M
import Data.Text (Text)
import Database.Selda (ID, fromId)
import System.IO.Unsafe
import qualified Notifier
import DB hiding (nextQuestion)

data Event
  = NextQuestion Question [Alt]
  | ShowStats Question [Alt]
  | QuizDone
  | AnswerReceived Int

type NotifierMap = M.IntMap (Notifier.Notifier Event)

data EventManager = EM
  { notifiers :: IORef NotifierMap
  }

new :: MonadIO m => m EventManager
new = liftIO $ EM <$> newIORef M.empty

await :: MonadIO m => EventManager -> ID Quiz -> m Event
await m qid = do
  m <- liftIO $ atomicModifyIORef' (notifiers m) (addNotifierIfMissing qid)
  let Just notifier = M.lookup (fromId qid) m
  Notifier.await notifier

addNotifierIfMissing :: ID Quiz -> NotifierMap -> (NotifierMap, NotifierMap)
addNotifierIfMissing qid m = unsafePerformIO $
  case M.lookup (fromId qid) m of
    Just notifier ->
      pure (m, m)
    _             -> do
      n <- Notifier.new
      let m' = M.insert (fromId qid) n m
      pure (m', m')

notify :: MonadIO m => EventManager -> ID Quiz -> Event -> m ()
notify m qid n = do
  mnotifier <- liftIO $ atomicModifyIORef' (notifiers m) $ \m ->
    (M.delete (fromId qid) m, M.lookup (fromId qid) m)
  maybe (pure ()) (flip Notifier.ping n) mnotifier
