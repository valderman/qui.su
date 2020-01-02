{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Backend.Tables
  ( module DB
  , users, quizzes, questions, alts
  ) where
import Database.Selda
import DB

users :: Table User
users = table "users"
  [ #userId :- autoPrimary
  , #googleId :- unique
  , #googleId :- index
  , #userEmail :- unique
  ]

quizzes :: Table Quiz
quizzes = table "quizzes"
  [ #quizId :- autoPrimary
  , #quizUrl :- unique
  , #ownerId :- foreignKey users #userId
  , #nextQuestion :- foreignKey questions #questionId
  ]

questions :: Table Question
questions = table "questions"
  [ #questionId :- autoPrimary
  , #quizId :- foreignKey quizzes #quizId
  ]

alts :: Table Alt
alts = table "alts"
  [ #altId :- autoPrimary
  , #questionId :- foreignKey questions #questionId
  ]
