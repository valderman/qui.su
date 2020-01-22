module Backend.Init (initDatabase) where
import Control.Monad
import Database.Selda
import Backend.Tables
import DB.Alternative as Alt (ordering, text, questionId)
import DB.Migrations
import Environment

initDatabase :: Env -> SeldaM s ()
initDatabase env = do
  tryCreateTable users
  tryCreateTable quizzes
  tryCreateTable questions
  tryCreateTable alts
  migrate