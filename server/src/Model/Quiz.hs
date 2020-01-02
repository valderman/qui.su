{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Model.Quiz where
import Data.Aeson (FromJSON, ToJSON)
import Data.Text as Text
import GHC.Generics

data Quiz = Quiz
  { name        :: Text
  , description :: Text
  , questions   :: [Question]
  } deriving (Show, Generic)
instance ToJSON Quiz
instance FromJSON Quiz

data Question = Question
  { title    :: Text
  , question :: Text
  , alts     :: [Alt]
  } deriving (Show, Generic)
instance ToJSON Question
instance FromJSON Question

data Alt = Alt
  { alternative :: Text
  , correct     :: Bool
  } deriving (Show, Generic)
instance ToJSON Alt
instance FromJSON Alt

quizToMarkdown :: Quiz -> Text
quizToMarkdown q = Text.strip $ Text.unlines
  [ name q
  , Text.replicate (Text.length (name q)) "="
  , description q
  , ""
  , Text.unlines [questionToMarkdown question <> "\n" | question <- questions q]
  ]

questionToMarkdown :: Question -> Text
questionToMarkdown q = Text.strip $ Text.unlines
  [ title q
  , Text.replicate (Text.length (title q)) "-"
  , question q
  , ""
  , Text.unlines [altToMarkdown no alt | (alt, no) <- Prelude.zip (alts q) [1..]]
  ]

altToMarkdown :: Int -> Alt -> Text
altToMarkdown n a = mconcat
  [ Text.pack (show n), ". "
  , Text.replace "\n" "    \n" (alternative a)
  , if correct a then " (correct)" else ""
  ]
