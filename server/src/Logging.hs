{-# LANGUAGE OverloadedLabels, OverloadedStrings, DeriveGeneric #-}
-- | Extremely basic and hacky logging.
module Logging (Severity (..), initLogger, Logging.log, orLog) where
import Control.Monad.Catch
import Data.Text (pack)
import Database.Selda
import Database.Selda.SQLite

logFile :: FilePath
logFile = "logs.sqlite"

data Severity = Debug | Info | Warning | Error | Fatal
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
instance SqlType Severity

data LogItem = LogItem
  { logItemId   :: ID LogItem
  , timestamp   :: UTCTime
  , severity    :: Severity
  , logTitle    :: Text
  , description :: Maybe Text
  } deriving Generic
instance SqlRow LogItem

logItems :: Table LogItem
logItems = table "logItems" [ #logItemId :- autoPrimary ]

log :: MonadIO m => Severity -> Text -> Maybe Text -> m ()
log sev title desc = liftIO $ withSQLite logFile $ do
  insert_ logItems [LogItem def def sev title desc]

-- | Perform the given action. If an exception is thrown,
--   log it using the given severity and title, and rethrow it.
orLog :: (MonadCatch m, MonadIO m) => m a -> (Severity, Text) -> m a
orLog act (sev, title) = do
  res <- try act
  case res of
    Right x                -> return x
    Left (SomeException e) -> do
      Logging.log sev title (Just $ pack $ displayException e)
      throwM e

initLogger :: IO ()
initLogger = withSQLite logFile $ do
  tryCreateTable logItems
