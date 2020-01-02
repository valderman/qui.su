{-# LANGUAGE DeriveGeneric #-}
module Token.Types
  ( Key, Issuer (..)
  , Jwt (..), UTCTime, NominalDiffTime, IntDate (..)
  , tokenSignAlgo
  ) where
import Data.Aeson
import Data.Text
import Data.Time
import GHC.Generics
import Jose.Jwa
import Jose.Jwk
import Jose.Jwt

type Key = Jwk

-- | A JWT token issuer, along with the required contents of the @aud@ claim,
--   if any.
data Issuer = Google Text | Hootsman Text
  deriving (Read, Show, Eq)

tokenSignAlgo :: JwsAlg
tokenSignAlgo = HS256
