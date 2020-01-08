{-# LANGUAGE TypeOperators, DataKinds, OverloadedStrings #-}
module Endpoints.Respondent (API, endpoints) where
import Data.Text (Text)
import Database.Selda (ID, toId, liftIO)
import Servant
import qualified Backend.Quiz as Backend
import Endpoints.Common
import qualified DB
import AppMonad

type Wait
  =  "wait"
  :> Capture "quizid" (ID DB.Quiz)
  :> Get '[JSON] NextQuestion
type GetQuestion
  =  "quiz"
  :> Capture "quizid" (ID DB.Quiz)
  :> Get '[JSON] NextQuestion
type GetQuestionById
  =  "question"
  :> Capture "questionid" (ID DB.Question)
  :> Get '[JSON] QuestionInfo
type Answer
  =  "answer"
  :> Capture "quizid" (ID DB.Quiz)
  :> ReqBody '[JSON] (ID DB.Alt)
  :> Post '[JSON] ()
type GetQuizId
  =  "resolve"
  :> Capture "url" Text
  :> Get '[JSON] (ID DB.Quiz)
type GetQuizUrl
  =  "url"
  :> Capture "url" (ID DB.Quiz)
  :> Get '[JSON] Text
type API
  =    Wait
  :<|> GetQuestion
  :<|> GetQuestionById
  :<|> Answer
  :<|> GetQuizId
  :<|> GetQuizUrl

endpoints :: Env -> Server API
endpoints
  =   public wait
  <|> public getQuestion
  <|> public getQuestionById
  <|> public answer
  <|> public getQuizIdByUrl
  <|> public getQuizUrlById

wait :: ID DB.Quiz -> AppM 'Anyone NextQuestion
wait qid = do
  notification <- waitEvent qid
  case notification of
    NextQuestion q alts -> do
      return $ Question $ markdownQuestionInfo (QuestionInfo q alts)
    ShowStats q alts -> do
      return $ Stats $ markdownQuestionInfo (QuestionInfo q alts)
    QuizDone -> do
      return Done
    _ -> do
      wait qid

getQuestion :: ID DB.Quiz -> AppM 'Anyone NextQuestion
getQuestion qid = do
  next <- runDB $ Backend.getNextQuestion qid
  return $ case next of
    Just (stats, (q, alts))
      | stats     -> Stats $ markdownQuestionInfo (QuestionInfo q alts)
      | otherwise -> Question $ markdownQuestionInfo (QuestionInfo q alts)
    _             -> Done

getQuestionById :: ID DB.Question -> AppM 'Anyone QuestionInfo
getQuestionById qid = do
  (q, alts) <- runDB $ Backend.getQuestionById qid
  return $ markdownQuestionInfo (QuestionInfo q alts)

getQuizIdByUrl :: Text -> AppM 'Anyone (ID DB.Quiz)
getQuizIdByUrl = runDB . Backend.getQuizIdByUrl

getQuizUrlById :: ID DB.Quiz -> AppM 'Anyone Text
getQuizUrlById = runDB . Backend.getQuizUrlById

answer :: ID DB.Quiz -> ID DB.Alt -> AppM 'Anyone ()
answer qid ans = do
  runDB $ Backend.answerQuestion qid ans
  raiseEvent qid (AnswerReceived ans)
