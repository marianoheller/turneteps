module App.Env where

import Prelude
import Data.Maybe (fromMaybe, maybe)
import Data.String as String
import Effect (Effect)
import Node.Process (lookupEnv)

getIds :: Effect (Array String)
getIds = do
  ids <- lookupEnv "appids"
  pure $ maybe [] (String.split (String.Pattern ",")) ids

getBaseUrl :: Effect String
getBaseUrl = do
  mBaseUrl <- lookupEnv "appbaseurl"
  pure $ fromMaybe "http://NO_BASE_URL" mBaseUrl

getToken :: Effect String
getToken = do
  mToken <- lookupEnv "usertoken"
  pure $ fromMaybe "NO-TOKEN" mToken
