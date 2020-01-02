{-# LANGUAGE TypeOperators, DataKinds, OverloadedStrings, DeriveGeneric #-}
{-# LANGUAGE DeriveGeneric #-}
module Endpoints.User (API, endpoints) where
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Database.Selda (ID, def)
import GHC.Generics
import Servant
import Endpoints.Common
import DB
import AppMonad as M
import Backend.User
import Token.Verify
import Token.Issue
import Token.Google (GoogleTokenInfo (sub, name, email))
import Token.Hootsman as T

data AuthResponse = AuthResponse
  { token   :: Jwt
  , user    :: DB.User
  , expires :: IntDate
  } deriving (Show, Generic)
instance ToJSON AuthResponse

type Auth = "auth" :> ReqBody '[JSON] ByteString :> Post '[JSON] AuthResponse

type API = Auth

endpoints :: Env -> Server API
endpoints = public auth

auth :: ByteString -> AppM 'M.Anyone AuthResponse
auth t = do
  clid <- getGoogleClientId
  keys <- getGoogleKeys
  result <- verifyToken (Google clid) keys t
  case result of
    Just info -> do
      u <- getOrCreateUser info
      (t, e) <- issueTokenFor u
      return (AuthResponse t u e)
    Nothing -> do
      throwError $ err401 { errBody = "Invalid Google token" }

issueTokenFor :: User -> AppM 'M.Anyone (Jwt, IntDate)
issueTokenFor u = do
  key <- getTokenKey
  issuer <- getAppId
  valid_for <- getTokenExpiry
  now <- liftIO getCurrentTime
  let ti = tokenInfo issuer now valid_for (userId u) (isAdmin u)
  t <- issue key ti
  return (t, T.exp ti)


getOrCreateUser :: GoogleTokenInfo -> AppM 'M.Anyone User
getOrCreateUser token = do
  muser <- runDB $ getUserByGoogleId (Token.Google.sub token)
  case muser of
    Just user -> return user
    _         -> runDB $ createUser $ DB.User
      { userId = def
      , googleId = Token.Google.sub token
      , userName = Token.Google.name token
      , userEmail = Token.Google.email token
      , isAdmin = False
      }
