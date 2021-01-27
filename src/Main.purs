module Main where

import Prelude

import App.BL as BL
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

main :: Effect (Promise String)
main =
  let
    initialResources apiUrl creds =
      sequential
        $ Tuple
        <$> parallel (Request.fetch $ Resources.misReservas apiUrl creds)
        <*> parallel (Request.fetch $ Resources.clases apiUrl creds)

    app :: Aff String
    app = do
      _ <- Dotenv.loadFile
      lowerBound <- liftEffect tomorrow
      targetDisciplinaId <- liftEffect Env.getTargetDisciplinaId
      { apiUrl, usersUrl } <- liftEffect Env.getBaseUrls
      Tuple loginInput basicAuth <- liftEffect Env.getAuthInfo
      creds <- Request.fetch $ Resources.login usersUrl basicAuth loginInput
      Tuple reservas clases <- initialResources apiUrl creds
      let
        targetClases = BL.process targetDisciplinaId lowerBound reservas clases
      results <- sequential $ for (unwrap targetClases) (parallel <<< Request.fetch <<< Resources.reserva apiUrl creds)
      pure $ (show targetClases) <> (show results)
  in
    fromAff app
