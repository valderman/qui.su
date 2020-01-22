{-# LANGUAGE DeriveGeneric #-}
module DB.Quiz
  ( Quiz (..)
  ) where
import DB.Common
import DB.User (User)
import {-# SOURCE #-} DB.Question (Question)

data Quiz = Quiz
  { quizId       :: ID Quiz
  , ownerId      :: ID User
  , name         :: Text
  , quizUrl      :: Text
  , nextQuestion :: Maybe (ID Question)
  , showingStats :: Bool
  , rawQuizText  :: Text
  } deriving Generic
instance SqlRow Quiz
instance FromJSON Quiz
instance ToJSON Quiz
