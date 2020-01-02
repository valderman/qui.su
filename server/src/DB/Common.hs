module DB.Common
  ( module Data.Aeson
  , module Database.Selda
  ) where
import Data.Aeson (ToJSON (..), FromJSON (..))
import Database.Selda
import Servant.API (FromHttpApiData (..))

instance ToJSON (ID a) where
  toJSON = toJSON . show . fromId
instance FromJSON (ID a) where
  parseJSON x = do
    x' <- parseJSON x
    case reads x' of
      [(x'', "")] -> return (toId x'')
      _           -> fail "invalid id"
instance FromHttpApiData (ID a) where
  parseUrlPiece = fmap toId . parseUrlPiece
