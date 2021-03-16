module App.Env where

import Prelude

import App.Data.Creds (BasicAuth, mkBasicAuth)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Console (log)
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

type AuthInfo
  = { loginInput :: LoginInput
    , basicAuth :: BasicAuth
    }

getAuthInfo :: Effect AuthInfo
getAuthInfo = do
  loginInput <- getLoginInfo
  basicAuth <- getBasicAuth
  pure { loginInput, basicAuth }

type BaseUrls
  = { apiUrl :: String
    , usersUrl :: String
    }

getBaseUrls :: Effect BaseUrls
getBaseUrls = do
  apiUrl <- getEnvByKey "baseApiUrl"
  usersUrl <- getEnvByKey "baseUsersUrl"
  pure { apiUrl, usersUrl }

getTargetDisciplinaId :: Effect Int
getTargetDisciplinaId = do
  disciplinaId <- getEnvByKey "disciplinaId"
  pure $ fromMaybe 3 $ fromString disciplinaId

type Env
  = { baseUrls :: BaseUrls
    , authInfo :: AuthInfo
    , disciplinaId :: Int
    }

getEnv :: Effect Env
getEnv = do
  authInfo <- getAuthInfo
  baseUrls <- getBaseUrls
  disciplinaId <- getTargetDisciplinaId
  _ <- log $ show baseUrls
  pure { authInfo, baseUrls, disciplinaId }
