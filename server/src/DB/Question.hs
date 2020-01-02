{-# LANGUAGE DeriveGeneric #-}
module DB.Question
  ( Question (..)
  ) where
import DB.Common
import DB.Quiz (Quiz)

data Question = Question
  { quizId       :: ID Quiz
  , questionId   :: ID Question
  , ordering     :: Int
  , text         :: Text
  } deriving Generic
instance SqlRow Question
instance FromJSON Question
instance ToJSON Question
