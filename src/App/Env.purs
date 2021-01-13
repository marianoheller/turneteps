module App.Env (getLoginInfo, LoginInput) where

import Prelude

import Data.Maybe (fromMaybe)
import Effect (Effect)
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
