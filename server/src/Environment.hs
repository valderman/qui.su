{-# LANGUAGE GADTs, TypeApplications, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Environment (Env (..), AuthToken (..), new, runDB) where
import Control.Exception (Exception, SomeException (..), try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString as BS
import Data.String
import Data.Text as Text
import Data.Text.Encoding as Text
import Data.Time
import Data.Typeable
import Database.Selda (SeldaM)
import Database.Selda.SQLite (SQLite, withSQLite)
import Servant (FromHttpApiData (..))
import System.Environment (getEnv)
import qualified EventManager as NM
import Token.Issue
import Token.Google

newtype AuthToken = AuthToken { unAT :: BS.ByteString }
  deriving Show

instance FromHttpApiData AuthToken where
  parseUrlPiece s =
    case Text.stripPrefix "Bearer " s of
      Just s' -> return (AuthToken $ Text.encodeUtf8 (Text.strip s'))
      _       -> Left "invalid authorization header"

-- TODO: Env = (State, Settings)
data Env = Env
  { eventManager   :: NM.EventManager
  , databaseFile   :: FilePath
  , authToken      :: Maybe AuthToken
  , tokenExpiry    :: NominalDiffTime
  , tokenKey       :: Key
  , googleClientId :: Text
  , googleKeys     :: [Key]
  , appId          :: Text
  , urlLength      :: Int
  , staticFileDir  :: FilePath
  , httpPort       :: Int
  }

data ParseException = ParseException
  deriving (Read, Show, Eq)
instance Exception ParseException

tryGetEnv :: forall a. (Typeable a, Read a) => String -> IO (Maybe a)
tryGetEnv key = do
    p <- try @SomeException $ getEnv key
    case p >>= readValue of
      Right value -> return (Just value)
      _           -> return Nothing
  where
    readValue :: String -> Either SomeException a
    readValue s =
      case eqT @a @String of
        Just Refl -> Right s
        _         -> case eqT @a @Text of
          Just Refl -> Right (fromString s)
          _         -> case eqT @a @BS.ByteString of
            Just Refl -> Right (fromString s)
            _         -> case reads s of
              [(value, "")] -> Right value
              _             -> Left (SomeException ParseException)

getEnvOr :: (Typeable a, Read a) => a -> String -> IO a
getEnvOr def key = maybe def id <$> tryGetEnv key

getPort :: IO Int
getPort = getEnvOr 8080 "HOOTSMAN_HTTP_PORT"

getStaticDir :: IO FilePath
getStaticDir = getEnvOr "./static" "HOOTSMAN_STATIC_DIR"

runDB :: MonadIO m => Env -> SeldaM SQLite a -> m a
runDB env = liftIO . withSQLite (databaseFile env)

new :: MonadIO m => m Env
new = liftIO $ do
  nm <- NM.new
  key <- genKey
  Just (GoogleKeys gkeys) <- decodeStrict <$> BS.readFile "googlekeys.json"
  port <- getPort
  staticDir <- getStaticDir
  return Env
    { eventManager = nm
    , databaseFile = "hootsman.sqlite"
    , googleClientId = "418225883139-3sec8j5pr44cqth5j1425clvoqomr2b0.apps.googleusercontent.com"
    , googleKeys = gkeys
    , authToken = Nothing
    , tokenExpiry = 12*3600
    , tokenKey = key
    , appId = "qui.su"
    , urlLength = 4
    , staticFileDir = staticDir
    , httpPort = port
    }
