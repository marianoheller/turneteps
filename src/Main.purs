module Main where

import Prelude
import App.Date as Date
import App.Request (fetch)
import App.Resources as Resources
import Data.Foldable (for_)
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

main :: Effect Unit
main =
  let
    eitherDate = Date.parseDateString "2020-12-12"
  in
    launchAff_ do
      _ <- Dotenv.loadFile
      for_ eitherDate (fetch <<< Resources.misTurnos)
      liftEffect $ log "🍝"
