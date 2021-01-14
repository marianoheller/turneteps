module App.Env (LoginInput, getLoginInfo, getAuthInfo, getBasicAuth) where

import Prelude
import App.Creds (BasicAuth, mkBasicAuth)
import Data.Bitraversable (bisequence)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Base64 (btoa)
import Node.Process (lookupEnv)

getEnvByKey :: String -> Effect String
getEnvByKey key = do
  mValue <- lookupEnv key
  pure $ fromMaybe (key <> " env var not found.") mValue

type LoginInput
  = { username :: String
    , password :: String
    , grant_type :: String
    }

getLoginInfo :: Effect LoginInput
getLoginInfo = do
  username <- getEnvByKey "username"
  password <- getEnvByKey "password"
  grant_type <- getEnvByKey "grant_type"
  pure { username, password, grant_type }

getBasicAuth :: Effect BasicAuth
getBasicAuth = do
  basicAuthCode <- getEnvByKey "basicAuthCode"
  pure $ mkBasicAuth $ btoa basicAuthCode

getAuthInfo :: Effect (Tuple LoginInput BasicAuth)
getAuthInfo = bisequence (Tuple getLoginInfo getBasicAuth)
