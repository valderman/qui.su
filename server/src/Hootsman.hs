{-# LANGUAGE TypeOperators, DataKinds, TypeApplications, OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Hootsman (hootsMain) where
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import Servant
import qualified Endpoints.Questioner as Questioner
import qualified Endpoints.Respondent as Respondent
import qualified Endpoints.User as User
import Environment as Env
import Backend.Init (initDatabase)
import Scheduler
import Jobs.UpdateGoogleKeys

data HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender OctetStream a => MimeRender HTML a where
  mimeRender _ val = mimeRender (Proxy @OctetStream) val

type API
  =    "api" :> (Questioner.API :<|> Respondent.API :<|> User.API)
  :<|> Get '[HTML] BS.ByteString
  :<|> Raw

endpoints :: Env -> Server API
endpoints env
  =   (Questioner.endpoints env
  :<|> Respondent.endpoints env
  :<|> User.endpoints env)
  :<|> serveIndex env
  :<|> serveDirectoryWebApp (staticFileDir env)

serveIndex :: Env -> Handler BS.ByteString
serveIndex env = liftIO $ do
  BS.readFile (staticFileDir env ++ "/" ++ "index.html")

app :: Env -> Application
app = serve (Proxy @API) . endpoints

hootsMain :: IO ()
hootsMain = do
  putStrLn "Getting fresh authentication keys from Google..."
  updateGoogleKeys "googlekeys.json"
  env <- Env.new
  putStrLn "Initialising database..."
  runDB env (initDatabase env)
  schedule (every 30 Minutes) (updateGoogleKeys "googlekeys.json")
  let port = httpPort env
  putStrLn ("Serving app on port " ++ show port ++ "!")
  run port (app env)
