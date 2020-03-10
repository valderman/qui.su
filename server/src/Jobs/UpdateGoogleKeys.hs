{-# LANGUAGE OverloadedStrings #-}
module Jobs.UpdateGoogleKeys where
import Data.ByteString.Lazy as BS
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Logging

updateGoogleKeys :: FilePath -> IO ()
updateGoogleKeys outfile = do
  Prelude.putStrLn "Updating Google keys..."
  mgr <- getGlobalManager
  let req = "https://www.googleapis.com/oauth2/v3/certs"
  response <- httpLbs req mgr `orLog` (Error, "Failed to get Google keys")
  if statusIsSuccessful (responseStatus response)
    then BS.writeFile outfile (responseBody response) `orLog` (Error, "Failed to get Google keys")
    else Logging.log Error "Failed to get Google keys" (Just $ decodeUtf8 $ toStrict $ responseBody response)
