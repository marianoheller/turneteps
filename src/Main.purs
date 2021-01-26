module Main where

import Prelude
import App.BL as BL
import App.Data.Clases as Clases
import App.Data.Reservas as Reservas
import App.Env as Env
import App.Request as Request
import App.Resources as Resources
import Control.Parallel (parallel, sequential)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (Error, runAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign.Date (tomorrow)

{- FIXME: date arg locale day -}
handleResult :: Either Error String -> Effect Unit
handleResult eResult = case eResult of
  Right result -> Console.log result
  Left err -> do
    Console.error "Task could not be finished successfully"
    Console.errorShow err

main :: Effect Unit
main =
  let
    initialResources creds =
      sequential
        $ Tuple
        <$> parallel (Request.fetch $ Resources.misReservas creds)
        <*> parallel (Request.fetch $ Resources.clases creds)

    app = do
      _ <- Dotenv.loadFile
      lowerBound <- liftEffect tomorrow
      Tuple loginInput basicAuth <- liftEffect Env.getAuthInfo
      creds <- Request.fetch $ Resources.login basicAuth loginInput
      Tuple reservas clases <- initialResources creds
      let
        targetClases = BL.process lowerBound (Reservas.groupPerDate reservas) (Clases.groupPerDate clases)
      results <- for (unwrap targetClases) (Request.fetch <<< Resources.reserva creds)
      pure $ show results <> show targetClases
  in
    runAff_ handleResult app
