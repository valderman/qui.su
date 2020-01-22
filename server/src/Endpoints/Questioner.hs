{-# LANGUAGE TypeOperators, DataKinds, OverloadedStrings, TypeApplications #-}
module Endpoints.Questioner (API, endpoints) where
import qualified Data.ByteString as BS
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Database.Selda (ID, toId)
import Servant
import System.Random
import qualified Backend.Quiz as Backend
import Endpoints.Common
import DB
import AppMonad as M
import QuizParser
import Token.Hootsman (sub)

type Notify
  =  "notify"
  :> Capture "quizid" (ID Quiz)
  :> Capture "question" (ID Question)
  :> Capture "stats" Bool
  :> AuthHeader
  :> Get '[JSON] NextQuestion
type QuizDone
  =  "end"
  :> Capture "quizid" (ID Quiz)
  :> AuthHeader
  :> Get '[JSON] ()
type QuizQuestions
  =  "questions"
  :> Capture "quizid" (ID Quiz)
  :> AuthHeader
  :> Get '[JSON] [ID Question]
type NewQuiz
  =  "quiz"
  :> Header "Content-Type" Text
  :> ReqBody '[OctetStream] BS.ByteString
  :> AuthHeader
  :> Post '[JSON] Quiz
type OverwriteQuiz
  =  "quiz"
  :> Capture "quizid" (ID Quiz)
  :> Header "Content-Type" Text
  :> ReqBody '[OctetStream] BS.ByteString
  :> AuthHeader
  :> Post '[JSON] ()
type QuizText
  =  "raw"
  :> Capture "quizid" (ID Quiz)
  :> AuthHeader
  :> Get '[JSON] Text
type Quizzes
  =  "quizzes"
  :> AuthHeader
  :> Get '[JSON] [Quiz]
type DeleteQuiz
  =  "delete"
  :> Capture "quizid" (ID Quiz)
  :> AuthHeader
  :> Get '[JSON] ()
type ResetAnswers
  =  "reset"
  :> Capture "quizid" (ID Quiz)
  :> AuthHeader
  :> Get '[JSON] ()
type AwaitAnswer
  =  "answers"
  :> Capture "quizid" (ID Quiz)
  :> AuthHeader
  :> Get '[JSON] Int

type API
  =    Notify
  :<|> QuizDone
  :<|> QuizQuestions
  :<|> NewQuiz
  :<|> OverwriteQuiz
  :<|> QuizText
  :<|> Quizzes
  :<|> DeleteQuiz
  :<|> ResetAnswers
  :<|> AwaitAnswer

endpoints :: Env -> Server API
endpoints
  =   authed notify
  <|> authed quizDone
  <|> authed quizQuestions
  <|> authed newQuiz
  <|> authed overwriteQuiz
  <|> authed getQuizText
  <|> authed getQuizzes
  <|> authed deleteQuiz
  <|> authed resetAnswers
  <|> authed awaitAnswer

quizQuestions :: ID Quiz -> AppM 'Anyone [ID Question]
quizQuestions = runDB . Backend.getQuizQuestionIds

getQuizText :: ID Quiz -> AppM 'Anyone Text
getQuizText = runDB . Backend.getQuizText

awaitAnswer :: ID Quiz -> AppM 'M.User Int
awaitAnswer qid = do
  notification <- waitEvent qid
  case notification of
    AnswerReceived answers -> return answers
    _                      -> awaitAnswer qid

notify :: ID Quiz -> ID Question -> Bool -> AppM 'M.User NextQuestion
notify qzid qid stats = do
  Just uid <- fmap sub <$> getAuthToken
  next <- runDB $ do
    Backend.setNextQuestion uid qzid qid stats
    Backend.getNextQuestion qzid
  case next of
    Just (False, (q, alts)) -> do
      raiseEvent qzid (NextQuestion q alts)
      let qi = markdownQuestionInfo (QuestionInfo q alts)
      return (Endpoints.Common.Question qi)
    Just (True, (q, alts)) -> do
      raiseEvent qzid (ShowStats q alts)
      let qi = markdownQuestionInfo (QuestionInfo q alts)
      return (Stats qi)
    _ -> do
      raiseEvent qzid QuizDone
      return Done

quizDone :: ID Quiz -> AppM 'M.User ()
quizDone qid = do
  Just uid <- fmap sub <$> getAuthToken
  text <- runDB $ Backend.finishQuiz uid qid
  raiseEvent qid QuizDone

overwriteQuiz :: ID Quiz -> Maybe Text -> BS.ByteString -> AppM 'M.User ()
overwriteQuiz qid _typ file = do
    Just uid <- fmap sub <$> getAuthToken
    case quiz of
      Just q -> runDB $ Backend.overwriteQuizFor uid qid q utf8File
      _      -> throwError $ err415 { errBody = "unable to parse quiz" }
  where
    utf8File = decodeUtf8 file
    quiz = parseQuiz utf8File

newQuiz :: Maybe Text -> BS.ByteString -> AppM 'M.User Quiz
newQuiz _typ file = do
    Just uid <- fmap sub <$> getAuthToken
    len <- getUrlLength
    case quiz of
      Just q -> retry 5 $ do
        -- retry up to five times in case of URL collision
        -- TODO: do something more sensible here
        url <- genQuizUrl len
        runDB $ Backend.createQuizFor uid url q utf8File
      _ ->
        throwError $ err415 { errBody = "unable to parse quiz" }
  where
    utf8File = decodeUtf8 file
    quiz = parseQuiz utf8File
    retry n m = do
      result <- m
      case result of
        Just x        -> return x
        _ | n > 0     -> retry (n-1) m
          | otherwise -> fail "unable to generate unique quiz url"

genUrlChar :: IO Char
genUrlChar = do
  choice <- randomRIO (0, 61)
  let i = case choice :: Int of
            x | x < 26    -> 0
              | x < 52    -> 1
              | otherwise -> 2
  randomRIO ([('a','z'), ('A','Z'), ('0','9')] !! i)

genQuizUrl :: MonadIO m => Int -> m Text
genQuizUrl n = fmap pack . liftIO $ sequence [genUrlChar | _ <- [1..n]]

getQuizzes :: AppM 'M.User [Quiz]
getQuizzes = do
  Just uid <- fmap sub <$> getAuthToken
  runDB $ Backend.getQuizzesFor uid

deleteQuiz :: ID Quiz -> AppM 'M.User ()
deleteQuiz qid = do
  Just uid <- fmap sub <$> getAuthToken
  runDB $ Backend.deleteQuizFor uid qid

resetAnswers :: ID Quiz -> AppM 'M.User ()
resetAnswers qid = do
  Just uid <- fmap sub <$> getAuthToken
  runDB $ Backend.resetQuizAnswers uid qid
