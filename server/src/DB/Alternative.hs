{-# LANGUAGE DeriveGeneric #-}
module DB.Alternative
  ( Alt (..)
  ) where
import DB.Common
import DB.Question (Question)

data Alt = Alt
  { altId      :: ID Alt
  , questionId :: ID Question
  , ordering   :: Int
  , text       :: Text
  , responses  :: Int
  , correct    :: Bool
  } deriving Generic
instance SqlRow Alt
instance FromJSON Alt
instance ToJSON Alt
