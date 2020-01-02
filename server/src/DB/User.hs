{-# LANGUAGE DeriveGeneric #-}
module DB.User (User (..)) where
import Database.Selda
import DB.Common

data User = User
  { userId    :: ID User
  , googleId  :: Text
  , userName  :: Text
  , userEmail :: Text
  , isAdmin   :: Bool
  } deriving (Show, Generic)
instance SqlRow User
instance FromJSON User
instance ToJSON User
