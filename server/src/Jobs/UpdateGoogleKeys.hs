{-# LANGUAGE OverloadedStrings #-}
module Jobs.UpdateGoogleKeys where
import Data.ByteString.Lazy as BS
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types

updateGoogleKeys :: FilePath -> IO ()
updateGoogleKeys outfile = do
  Prelude.putStrLn "Updating Google keys..."
  mgr <- getGlobalManager
  let req = "https://www.googleapis.com/oauth2/v3/certs"
  response <- httpLbs req mgr
  if statusIsSuccessful (responseStatus response)
    then BS.writeFile outfile (responseBody response)
    else Prelude.putStrLn "Failed to get Google keys!"
