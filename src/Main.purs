module Main where

import Prelude
import App.BL as BL
import App.Data.Clases as Clases
import App.Data.Date as Date
import App.Data.Reservas as Reservas
import App.Env as Env
import App.Request as Request
import App.Resources as Resources
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console

main :: Effect Unit
main =
  let
    app = do
      _ <- Dotenv.loadFile
      Tuple loginInput basicAuth <- liftEffect Env.getAuthInfo
      ahora <- liftEffect Date.now
      creds <- Request.fetch $ Resources.login basicAuth loginInput
      reservas <- Request.fetch $ Resources.misReservas creds
      clases <- Request.fetch $ Resources.clases creds
      pure $ show $ BL.getTargetClases ahora (Reservas.groupPerDateTime reservas) (Clases.groupPerDateTime clases)
  in
    runAff_ handleResult app

handleResult :: Either Error String -> Effect Unit
handleResult eResult = case eResult of
  Right result -> Console.log result
  Left err -> do
    Console.error "Task could not be finished successfully"
    Console.errorShow err
