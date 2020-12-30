module App.Request (fetch) where

import Prelude
import App.Resources (Endpoint)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Milkis (Response)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Node.Process (lookupEnv)
import Option as Option
import Record as Record
import Type.Row (class Union)

getTokenFromEnv :: Effect (Maybe String)
getTokenFromEnv = lookupEnv "usertoken"

_fetch :: forall options trash. Union options trash M.Options => M.URL -> { method :: M.Method | options } -> Aff Response
_fetch url opts = logger $ M.fetch nodeFetch url opts

logger :: Aff Response -> Aff Response
logger resAff = do
  let
    formatResponse :: Response -> String
    formatResponse res =
      let
        url = show $ M.url res

        status = show $ M.statusCode res

        corch str = "[" <> str <> "]"
      in
        intercalate " "
          $ [ (corch status)
            , url
            ]
  res <- resAff
  liftEffect $ log $ formatResponse res
  resAff

fetch :: Endpoint -> Aff Response
fetch endpoint = do
  token <- liftEffect getTokenFromEnv
  let
    record = Option.recordToRecord endpoint

    baseConfig =
      { method: record.method
      , headers: M.makeHeaders { "X-Auth": (fromMaybe "NO-TOKEN" token) }
      }

    goFetch = _fetch record.url

    goFetchWithBody = _fetch record.url
  case record.body of
    Nothing -> goFetch baseConfig
    Just body -> goFetchWithBody $ Record.merge baseConfig { body }
