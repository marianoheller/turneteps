module App.Env (getLoginInfo, LoginInput, getBasicAuth) where

import Prelude
import App.Creds (BasicAuth, mkBasicAuth)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Foreign.Base64 (btoa)
import Node.Process (lookupEnv)

getLoginKey :: String -> Effect String
getLoginKey key = do
  mValue <- lookupEnv key
  pure $ fromMaybe (key <> " env var not found.") mValue

type LoginInput
  = { username :: String
    , password :: String
    , grant_type :: String
    }

getLoginInfo :: Effect LoginInput
getLoginInfo = do
  username <- getLoginKey "username"
  password <- getLoginKey "password"
  grant_type <- getLoginKey "grant_type"
  pure { username, password, grant_type }

getBasicAuth :: Effect BasicAuth
getBasicAuth = do
  basicAuthCode <- getLoginKey "basicAuthCode"
  pure $ mkBasicAuth $ btoa basicAuthCode
