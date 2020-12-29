module App.Request where

import Prelude

import App.Resources (Endpoint)
import Data.Maybe (Maybe, fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Object as Object
import Milkis (Response)
import Milkis as Milkis
import Milkis.Impl.Node (nodeFetch)
import Node.Process (lookupEnv)
import Option as Option
import Prim.Row (class Union)

getTokenFromEnv :: Effect (Maybe String)
getTokenFromEnv = lookupEnv "usertoken"

authHeaderKey :: String
authHeaderKey = "X-Auth"

_fetch ::
  forall options trash.
  Union
    options
    trash
    Milkis.Options =>
  Milkis.URL ->
  { method :: Milkis.Method
  | options
  } ->
  Aff Response
_fetch = Milkis.fetch nodeFetch

fetch :: Endpoint -> Aff Response
fetch endpoint = do
  let
    record = Option.recordToRecord endpoint

    body = fromMaybe "" record.body
  token <- liftEffect getTokenFromEnv
  _fetch record.url
    { method: record.method
    , body
    , headers: Object.singleton authHeaderKey (fromMaybe "NO-TOKEN" token)
    }
