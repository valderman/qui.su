module Token.Issue
  ( module Token.Types
  , issue, genKey
  , utcTimeToPOSIXSeconds, getCurrentTime, addUTCTime
  ) where
import Control.Monad.IO.Class
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (isJust)
import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Jose.Jwa
import Jose.Jwk
import Jose.Jwt
import Token.Types
import Token.Hootsman

genKey :: MonadIO m => m Key
genKey = liftIO $ do
  t <- getCurrentTime
  generateSymmetricKey 32 (UTCKeyId t) Sig (Just (Signed tokenSignAlgo))

issue :: (A.ToJSON sub, MonadIO m) => Key -> TokenInfo sub -> m Jwt
issue key ti = liftIO $ do
  t <- getCurrentTime
  let enc = JwsEncoding tokenSignAlgo
  tok <- encode [key] enc (Claims $ BS.toStrict $ A.encode ti)
  case tok of
    Left e      -> error ("BUG: " ++ show e)
    Right token -> return token
