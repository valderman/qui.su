{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Backend.User
  ( getUserById, getUserByGoogleId
  , createUser
  ) where
import Data.Maybe
import Database.Selda
import Backend.Tables

getUserByGoogleId :: Text -> SeldaM s (Maybe User)
getUserByGoogleId gid = fmap listToMaybe . query $ do
  u <- select users
  restrict (u ! #googleId .== literal gid)
  return u

getUserById :: ID User -> SeldaM s (Maybe User)
getUserById uid = fmap listToMaybe . query $ do
  u <- select users
  restrict (u ! #userId .== literal uid)
  return u

createUser :: User -> SeldaM s User
createUser u = do
  uid <- insertWithPK users [u]
  return $ u { userId = uid }
