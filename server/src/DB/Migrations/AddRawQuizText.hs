{-# LANGUAGE OverloadedStrings, OverloadedLabels, DeriveGeneric #-}
module DB.Migrations.AddRawQuizText (migrate) where
import Database.Selda
import Database.Selda.Migrations hiding (migrate)
import Database.Selda.Unsafe (cast)
import Backend.Tables

data Quiz_NoRawQuizText = Quiz
  { quizId       :: ID Quiz_NoRawQuizText
  , ownerId      :: ID User
  , name         :: Text
  , quizUrl      :: Text
  , nextQuestion :: Maybe (ID Question)
  , showingStats :: Bool
  } deriving Generic
instance SqlRow Quiz_NoRawQuizText

quizzes_noRawQuizText :: Table Quiz_NoRawQuizText
quizzes_noRawQuizText = table "quizzes"
  [ #quizId :- autoPrimary
  , #quizUrl :- unique
  , #ownerId :- foreignKey users #userId
  , #nextQuestion :- foreignKey questions #questionId
  ]


migrate :: [Migration b]
migrate =
  [ Migration quizzes_noRawQuizText quizzes $ \r -> pure $ new
      [ #rawQuizText := ""
      , #quizId := cast (r ! #quizId)
      , #ownerId := r ! #ownerId
      , #name := r ! #name
      , #quizUrl := r ! #quizUrl
      , #nextQuestion := r ! #nextQuestion
      , #showingStats := r ! #showingStats
      ]
  ]