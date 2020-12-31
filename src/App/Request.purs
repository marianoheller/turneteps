module App.Request (fetch) where

import Prelude
import App.Env as Env
import App.Resources (Resource(..))
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as Argonaut
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Milkis (Response)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Option as Option
import Record as Record
import Type.Row (class Union)

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
      in
        intercalate " "
          $ [ "[" <> status <> "]"
            , url
            ]
  res <- resAff
  liftEffect $ log $ formatResponse res
  resAff

fetch :: forall a. DecodeJson a => Resource a -> Aff (Either String a)
fetch (Resource resouce) = do
  token <- liftEffect Env.getToken
  let
    record = Option.recordToRecord resouce

    baseConfig =
      { method: record.method
      , headers: M.makeHeaders { "X-Authorization": token }
      }

    decode = (lmap show <<< Argonaut.decodeJson)
  rawResponse <- case record.body of
    Nothing -> _fetch record.url baseConfig
    Just body -> _fetch record.url $ Record.merge baseConfig { body }
  eitherJson <- (map Argonaut.jsonParser) $ M.text rawResponse
  pure $ decode =<< eitherJson
