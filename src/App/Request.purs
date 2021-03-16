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
import Effect.Class.Console as Console
import Effect.Unsafe (unsafePerformEffect)
import Milkis (Response)
import Milkis as M
import Milkis.Impl.Node (nodeFetch)
import Record as Record
import Type.Row (class Union)

_fetch :: forall options trash. Union options trash M.Options => M.URL -> { method :: M.Method | options } -> Aff Response
_fetch url opts = logger <$> (M.fetch nodeFetch url opts)

logger :: Response -> Response
logger res =
  let
    formatResponse :: Response -> String
    formatResponse r =
      let
        url = show $ M.url r

        status = show $ M.statusCode r
      in
        intercalate " "
          $ [ "[" <> status <> "]"
            , url
            ]

    -- TODO: improve this, remove unsafe
    _ = unsafePerformEffect $ Console.log $ formatResponse res
  in
    res

fetch :: forall a. DecodeJson a => Resource a -> Aff a
fetch (Resource resource) = do
  let
    url = M.URL resource.url

    baseConfig =
      { method: resource.method
      , headers: resource.headers
      }

    jsonParser = lmap error <<< Argonaut.jsonParser

    decode = lmap (error <<< show) <<< Argonaut.decodeJson

    timingKey = "Parsing: " <> resource.url
  rawResponse <- case resource.body of
    Nothing -> _fetch url baseConfig
    Just body -> _fetch url $ Record.merge baseConfig { body }
  eitherJson <- (map jsonParser) $ M.text rawResponse
  liftEffect $ Console.time timingKey
  case decode =<< eitherJson of
    Right parsed -> do
      liftEffect $ Console.timeEnd timingKey
      pure parsed
    Left err -> throwError err
