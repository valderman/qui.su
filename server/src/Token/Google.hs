{-# LANGUAGE DeriveGeneric #-}
module Token.Google (GoogleTokenInfo (..), GoogleKeys (..)) where
import Data.Aeson
import Data.Text
import GHC.Generics
import Token.Types (Key)

newtype GoogleKeys = GoogleKeys { keys :: [Key] }
  deriving (Show, Generic)
instance ToJSON GoogleKeys
instance FromJSON GoogleKeys

data GoogleTokenInfo = GoogleTokenInfo
  { sub :: Text
  , name :: Text
  , email :: Text
  } deriving (Show, Generic)
instance ToJSON GoogleTokenInfo
instance FromJSON GoogleTokenInfo
