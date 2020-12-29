module Main where

import Prelude

import App.Request (fetch)
import App.Resources as Resources
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)

main :: Effect Unit
main =
  launchAff_ do
    _ <- Dotenv.loadFile
    _ <- fetch $ Resources.misTurnos "2020-12-12"
    liftEffect $ log "🍝"
