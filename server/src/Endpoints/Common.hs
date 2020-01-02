{-# LANGUAGE DeriveGeneric, DataKinds #-}
module Endpoints.Common where
import Data.Aeson (ToJSON (..), FromJSON (..))
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import GHC.Generics
import Servant.API
import qualified DB
import Environment (AuthToken)
import QuizParser (toHtml)
import qualified DB.Alternative as Alt
import qualified DB.Question as Q

data NextQuestion = Done | Question QuestionInfo | Stats QuestionInfo
  deriving Generic
instance ToJSON NextQuestion
instance FromJSON NextQuestion

data QuizInfo = QuizInfo
  { quiz      :: DB.Quiz
  , questions :: [QuestionInfo]
  } deriving Generic
instance ToJSON QuizInfo
instance FromJSON QuizInfo

data QuestionInfo = QuestionInfo
  { question     :: DB.Question
  , alternatives :: [DB.Alt]
  } deriving Generic
instance ToJSON QuestionInfo
instance FromJSON QuestionInfo

instance ToJSON BS.ByteString where
  toJSON = toJSON . decodeUtf8
instance FromJSON BS.ByteString where
  parseJSON = fmap encodeUtf8 . parseJSON

type AuthHeader = Header "Authentication" AuthToken

markdownQuestionInfo :: QuestionInfo -> QuestionInfo
markdownQuestionInfo (QuestionInfo q alts) = QuestionInfo q' alts'
  where
    q' = q { Q.text = toHtml (Q.text q) }
    alts' = map markdownAlt alts
    markdownAlt a = a { Alt.text = toHtml (Alt.text a) }
