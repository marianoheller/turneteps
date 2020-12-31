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
getBaseUrl = (map $ fromMaybe "https://BASE_URL") $ lookupEnv "appbaseurl"

getToken :: Effect String
getToken = (map $ fromMaybe "NO-TOKEN") $ lookupEnv "usertoken"
