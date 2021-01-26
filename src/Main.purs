module Main where

import Prelude
import App.BL as BL
import App.Data.Clases as Clases
import App.Data.Reservas as Reservas
import App.Env as Env
import App.Request as Request
import App.Resources as Resources
import Control.Parallel (parallel, sequential)
import Control.Promise (Promise, fromAff)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Date (tomorrow)

{- FIXME: date arg locale day -}
main :: Effect (Promise String)
main =
  let
    initialResources creds =
      sequential
        $ Tuple
        <$> parallel (Request.fetch $ Resources.misReservas creds)
        <*> parallel (Request.fetch $ Resources.clases creds)

    app :: Aff String
    app = do
      _ <- Dotenv.loadFile
      lowerBound <- liftEffect tomorrow
      Tuple loginInput basicAuth <- liftEffect Env.getAuthInfo
      creds <- Request.fetch $ Resources.login basicAuth loginInput
      Tuple reservas clases <- initialResources creds
      let
        targetClases = BL.process lowerBound (Reservas.groupPerDate reservas) (Clases.groupPerDate clases)
      results <- sequential $ for (unwrap targetClases) (parallel <<< Request.fetch <<< Resources.reserva creds)
      pure $ show results <> show targetClases
  in
    fromAff app
