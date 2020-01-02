{-# LANGUAGE DeriveGeneric #-}
module Token.Hootsman (TokenInfo (..), tokenInfo) where
import Prelude hiding (exp)
import Data.Aeson
import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import GHC.Generics
import Token.Types (Key)
import Jose.Jwt

tokenInfo :: Text -> UTCTime -> NominalDiffTime -> a -> Bool -> TokenInfo a
tokenInfo issuer now valid_for uid is_admin = TokenInfo
  { iss   = issuer
  , sub   = uid
  , admin = is_admin
  , exp   = IntDate (utcTimeToPOSIXSeconds (addUTCTime valid_for now))
  , iat   = IntDate (utcTimeToPOSIXSeconds now)
  }

-- | Note: @sub@ should serialize to a JSON string.
data TokenInfo sub = TokenInfo
  { iss   :: Text
  , sub   :: sub
  , exp   :: IntDate
  , iat   :: IntDate
  , admin :: Bool
  } deriving (Show, Generic)
instance ToJSON sub => ToJSON (TokenInfo sub)
instance FromJSON sub => FromJSON (TokenInfo sub)
