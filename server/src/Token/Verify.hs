{-# LANGUAGE TypeApplications, OverloadedStrings, ScopedTypeVariables #-}
module Token.Verify
  ( module Token.Types
  , verifyToken, verifyToken_
  ) where
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, decodeStrict)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Data.Text
import Data.Text.Encoding
import Data.Time
import Data.Time.Clock.POSIX
import Jose.Jwa
import Jose.Jwk
import Jose.Jwt
import Token.Types
import Logging

verifyToken_ :: forall m. MonadIO m => Issuer -> [Key] -> BS.ByteString -> m Bool
verifyToken_ issuer keys token = do
  isJust <$> verifyToken @m @JwtClaims issuer keys token

verifyToken :: forall m a. (MonadIO m, Show a, FromJSON a)
            => Issuer -> [Key] -> BS.ByteString -> m (Maybe a)
verifyToken issuer keys token = liftIO $ do
    result <- decode keys encoding token
    case result of
      Left _ -> do
        Logging.log Debug "Unable to decode token" (Just $ decodeUtf8 token)
        return Nothing
      Right (Jws (hd, claims)) -> do
        now <- getCurrentTime
        if maybe False (verifyClaims issuer now) (decodeStrict claims)
          then do
            return (decodeStrict claims)
          else do
            Logging.log Debug "Unable to verify token claims" (Just $ decodeUtf8 token)
            return Nothing
  where
    encoding =
      case issuer of
        Google _   -> Just (JwsEncoding RS256)
        Hootsman _ -> Just (JwsEncoding tokenSignAlgo)

verifyClaims :: Issuer -> UTCTime -> JwtClaims -> Bool
verifyClaims issuer t claims = and
    [ maybe True (> t') (jwtExp claims)
    , maybe True (< t') (jwtNbf claims)
    , verifyIssuer issuer claims
    ]
  where
    t' = IntDate (utcTimeToPOSIXSeconds t)

verifyIssuer :: Issuer -> JwtClaims -> Bool
verifyIssuer (Google aud) claims =
  verifyAud aud claims && verifyIss "accounts.google.com" claims
verifyIssuer (Hootsman iss) claims =
  verifyIss iss claims -- we don't issue tokens for anyone else

verifyAud :: Text -> JwtClaims -> Bool
verifyAud aud = maybe False (aud `elem`) . jwtAud

verifyIss :: Text -> JwtClaims -> Bool
verifyIss iss = maybe False (`elem` issuers) . jwtIss
  where issuers = [iss, "https://" <> iss]
