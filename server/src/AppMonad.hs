{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, KindSignatures, DataKinds, InstanceSigs #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances #-}
module AppMonad
  ( AppM, MonadApp (..), MonadEnv (..), Privilege (..), Priv (..), E.Env
  , Event (..), Endpoint (..), AuthedEndpoint (..), MonadIO (..)
  , E.AuthToken (..)
  , (<|>), forbidden, getAuthedUser
  ) where
import Control.Monad.Error
import Control.Monad.Fail
import Control.Monad.IO.Class
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BS
import Data.Proxy
import Data.String
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime)
import Database.Selda
import Database.Selda.SQLite
import GHC.TypeLits as TL
import Servant
import qualified Environment as E
import EventManager
import DB.User (User (isAdmin))
import DB.Quiz (Quiz)
import Backend.User (getUserById)
import Token.Verify
import Token.Hootsman

import Debug.Trace
tr x = trace (show x) x

newtype AppM (p :: Priv) a = AppM { unApp :: ReaderT E.Env Handler a }
  deriving (Functor, Applicative, Monad, MonadEnv, MonadIO, MonadError ServerError)

class Privilege (p :: Priv) where
  guardPriv :: (MonadIO m, MonadEnv m) => Proxy p -> m ()

instance Privilege 'Anyone where
  guardPriv _ = return ()

instance Privilege 'User where
  guardPriv _ = do
    muser <- getAuthedUser
    case muser of
      Just _ -> return ()
      _      -> forbidden "only logged in users can do that"

instance Privilege 'Admin where
  guardPriv _ = do
    muser <- getAuthedUser
    case muser of
      Just user | isAdmin user -> return ()
      _                        -> forbidden "only admins can do that"

forbidden :: MonadEnv m => Text -> m ()
forbidden text = liftHandler $ do
  throwError $ err403 { errBody = BS.fromStrict (encodeUtf8 text) }

data Priv = Anyone | User | Admin

class MonadApp m where
  runAppM :: Privilege p => E.Env -> m p a -> Handler a

instance MonadFail (AppM p) where
  fail s = throwError $ err500 { errBody = fromString s }

instance MonadApp AppM where
  runAppM :: forall p a. Privilege p => E.Env -> AppM p a -> Handler a
  runAppM env m = flip runReaderT env $ guardPriv (Proxy :: Proxy p) >> unApp m

class Monad m => MonadEnv m where
  getGoogleClientId :: m Text
  getGoogleKeys :: m [Key]
  getTokenKey :: m Key
  getTokenExpiry :: m NominalDiffTime
  getAppId :: m Text
  getAuthToken :: m (Maybe (TokenInfo (ID User)))
  getUrlLength :: m Int
  runDB :: SeldaM SQLite a -> m a
  waitEvent :: ID Quiz -> m Event
  raiseEvent :: ID Quiz -> Event -> m ()
  liftHandler :: Handler a -> m a

instance MonadEnv (ReaderT E.Env Handler) where
  getGoogleClientId = E.googleClientId <$> ask
  getGoogleKeys = E.googleKeys <$> ask
  getTokenKey = E.tokenKey <$> ask
  getTokenExpiry = E.tokenExpiry <$> ask
  getAppId = E.appId <$> ask
  getUrlLength = E.urlLength <$> ask
  getAuthToken = do
    e <- ask
    case E.authToken e of
      Just (E.AuthToken t) -> verifyToken (Hootsman (E.appId e)) [E.tokenKey e] t
      _                    -> return Nothing
  runDB m = do
    env <- ask
    E.runDB env m
  waitEvent qid = do
    em <- E.eventManager <$> ask
    await em qid
  raiseEvent qid evt = do
    em <- E.eventManager <$> ask
    notify em qid evt
  liftHandler = lift

getAuthedUser :: (MonadIO m, MonadEnv m) => m (Maybe User)
getAuthedUser = do
  mt <- getAuthToken
  case mt of
    Nothing -> do
      pure Nothing
    Just t -> do
      mti <- getAuthToken
      maybe (pure Nothing) (runDB . getUserById . sub) mti

type family Public a where
  Public (a -> b)         = a -> Public b
  Public (AppM 'Anyone a) = Handler a
  Public (AppM p a)       = TypeError (TL.Text "Endpoint has stricter permissions than 'Anyone: " :<>: ShowType p)

type family Authed a where
  Authed (a -> b)   = a -> Authed b
  Authed (AppM p a) = Maybe E.AuthToken -> Handler a

class Endpoint a where
  public :: a -> E.Env -> Public a

class AuthedEndpoint a where
  authed :: a -> E.Env -> Authed a

instance Endpoint b => Endpoint (a -> b) where
  public f env = \x -> public (f x) env

instance (MonadApp m, Public (m 'Anyone a) ~ Handler a) => Endpoint (m 'Anyone a) where
  public = flip runAppM

instance AuthedEndpoint b => AuthedEndpoint (a -> b) where
  authed f env = \x -> authed (f x) env

instance (MonadApp m, Privilege p, Authed (m p a) ~ (Maybe E.AuthToken -> Handler a)) =>
  AuthedEndpoint (m p a) where
  authed m env t = runAppM (env {E.authToken = t}) m

(<|>) :: (t -> a) -> (t -> b) -> (t -> (a :<|> b))
a <|> b = \x -> a x :<|> b x
infixr 3 <|>
