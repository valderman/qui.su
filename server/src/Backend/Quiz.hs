{-# LANGUAGE OverloadedStrings, OverloadedLabels, TupleSections #-}
module Backend.Quiz
  ( getQuizQuestionIds, getQuestionById, getNextQuestion, answerQuestion
  , finishQuiz, setNextQuestion
  , createQuizFor, deleteQuizFor, resetQuizAnswers
  , getQuizzesFor, getQuizIdByUrl, getQuizUrlById
  ) where
import Control.Monad
import Data.List
import Database.Selda
import Database.Selda.Unsafe (fun2)
import Backend.Tables as DB
import qualified DB.Question as Question
import qualified DB.Alternative as Alt
import qualified Model.Quiz as MQ

min2, max2 :: SqlOrd a => Col s a -> Col s a -> Col s a
min2 = fun2 "min"
max2 = fun2 "max"

clamp :: SqlOrd a => Col s a -> Col s a -> Col s a -> Col s a
clamp lower upper = max2 lower . min2 upper

getQuestionById :: ID Question -> SeldaM s (Question, [Alt])
getQuestionById qid = do
  [q] <- query $ do
    select questions `suchThat` (\q -> q ! #questionId .== literal qid)
  alts <- query $ do
    select alts `suchThat` (\a -> a ! #questionId .== literal qid)
  return (q, alts)

getNextQuestion :: ID Quiz -> SeldaM s (Maybe (Bool, (Question, [Alt])))
getNextQuestion qid = do
  [q] <- query $ select quizzes `suchThat` (\q -> literal qid .== q ! #quizId)
  case nextQuestion q of
    Just next -> (Just . (showingStats q,)) <$> getQuestionById next
    _         -> return Nothing

answerQuestion :: ID Quiz -> ID Alt -> SeldaM s ()
answerQuestion qid aid = transaction $ do
  [is_next] <- query $ do
    a <- select alts `suchThat` (\a -> a ! #altId .== literal aid)
    q <- select questions
    restrict (q ! #questionId .== a ! #questionId)
    qz <- select quizzes
    restrict (qz ! #quizId .== q ! #quizId .&& literal qid .== q ! #quizId)
    return (just (a ! #questionId) .== qz ! #nextQuestion)
  when is_next $ do
    update_ alts (\a -> a ! #altId .== literal aid)
                 (\a -> a `with` [#responses += 1])

finishQuiz :: ID User -> ID Quiz -> SeldaM s ()
finishQuiz oid qid = do
  update_ quizzes (\q -> q ! #quizId .== literal qid .&&
                         q ! #ownerId .== literal oid)
                  (\q -> q `with` [#nextQuestion := null_])

setNextQuestion :: ID User -> ID Quiz -> ID Question -> Bool -> SeldaM s ()
setNextQuestion oid qzid qid stats = transaction $ do
  update_ quizzes (\q -> q ! #quizId .== literal qzid .&&
                         q ! #ownerId .== literal oid)
                  (\q -> q `with`
                    [ #nextQuestion := just (literal qid)
                    , #showingStats := literal stats
                    ])

-- | Creates the given quiz for the given user, provided that the quiz has
--   at least one question.
--   Returns @Nothing@ if a quiz already exists with the given URL.
createQuizFor :: ID User -> Text -> MQ.Quiz -> SeldaM s (Maybe Quiz)
createQuizFor oid url quiz = transaction $ do
    when (null $ MQ.questions quiz) $ do
      fail "quiz needs at least one question"
    qids <- query $ do
      #quizId `from` select quizzes `suchThat` \q -> q ! #quizUrl .== literal url
    case qids of
      [] -> do
        qid <- insertWithPK quizzes [quiz']
        zipWithM_ (insertQuestion qid) (MQ.questions quiz) [0..]
        return (Just $ quiz' {quizId = qid})
      _ -> do
        return Nothing
  where
    quiz' = DB.Quiz
      { quizId = def
      , ownerId = oid
      , nextQuestion = Nothing
      , quizUrl = url
      , name = MQ.name quiz
      , showingStats = False
      }
    insertQuestion qid q n = do
      question_id <- insertWithPK questions [Question qid def n (MQ.question q)]
      insert_ alts $ flip map (MQ.alts q) $ \a ->
        Alt def question_id 0 (MQ.alternative a) 0 (MQ.correct a)

-- | Deletes the given quiz, provided that it is owned by the given user.
deleteQuizFor :: ID User -> ID Quiz -> SeldaM s ()
deleteQuizFor oid qid = transaction $ do
  quiz <- query $ do
    select quizzes `suchThat` \q -> q ! #quizId .== literal qid .&&
                                    q ! #ownerId .== literal oid
  unless (null quiz) $ do
    update quizzes (\q -> q ! #quizId .== literal qid)
                   (\q -> q `with` [#nextQuestion := null_])
    qids <- map literal <$> getQuizQuestionIds qid
    deleteFrom_ alts (\a -> a ! #questionId `isIn` qids)
    deleteFrom_ questions (\a -> a ! #quizId .== literal qid)
    deleteFrom_ quizzes (\a -> a ! #quizId .== literal qid)

getQuizQuestionIds :: ID Quiz -> SeldaM s [ID Question]
getQuizQuestionIds qid = query $ do
  q <- select questions `suchThat` \q -> q ! #quizId .== literal qid
  order (q ! #ordering) ascending
  return (q ! #questionId)

getQuizzesFor :: ID User -> SeldaM s [Quiz]
getQuizzesFor uid = query $ do
  q <- select quizzes
  restrict (q ! #ownerId .== literal uid)
  order (q ! #quizId) ascending
  return q

getQuizIdByUrl :: Text -> SeldaM s (ID Quiz)
getQuizIdByUrl url = fmap head . query $ do
  q <- select quizzes
  restrict (q ! #quizUrl .== literal url)
  return (q ! #quizId)

getQuizUrlById :: ID Quiz -> SeldaM s Text
getQuizUrlById qid = fmap head . query $ do
  q <- select quizzes
  restrict (q ! #quizId .== literal qid)
  return (q ! #quizUrl)

-- | Resets all answers to the given quiz.
resetQuizAnswers :: ID User -> ID Quiz -> SeldaM s ()
resetQuizAnswers oid qid = transaction $ do
  quiz <- query $ do
    select quizzes `suchThat` \q -> q ! #quizId .== literal qid .&&
                                    q ! #ownerId .== literal oid
  qids <- map literal <$> getQuizQuestionIds qid
  update_ alts (\a -> a ! #questionId `isIn` qids)
               (\a -> a `with` [#responses := 0])
