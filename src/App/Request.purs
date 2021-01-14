module App.Request where

import Prelude
import App.Resources (Resource(..))
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as Argonaut
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, throwError)
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

fetch :: forall a. DecodeJson a => Resource a -> Aff a
fetch (Resource resouce) = do
  let
    record = Option.recordToRecord resouce

    url = M.URL record.url

    baseConfig =
      { method: record.method
      , headers: record.headers
      }

    jsonParser = lmap error <<< Argonaut.jsonParser

    decode = lmap (error <<< show) <<< Argonaut.decodeJson
  rawResponse <- case record.body of
    Nothing -> _fetch url baseConfig
    Just body -> _fetch url $ Record.merge baseConfig { body }
  eitherJson <- (map jsonParser) $ M.text rawResponse
  case decode =<< eitherJson of
    Right parsed -> pure parsed
    Left err -> throwError err
