module App where

import Prelude

import App.Utils as Utils
import App.Data.Clases (Clases)
import App.Data.Creds (BasicAuth, Creds)
import App.Data.ReservaResult (ReservaResult)
import App.Data.Reservas (Reservas)
import App.Env (LoginInput, Env)
import App.Env as Env
import App.Request as Request
import App.Resources as Resources
import Control.Monad.Free (Free, liftF)
import Data.Date (Date)
import Effect.Aff (Aff)
import Foreign.Date (tomorrow)
import Effect.Class (liftEffect)
import Control.Parallel (parallel, sequential)
import Data.Newtype (unwrap)
import Data.Traversable (for)

data AppF :: Type -> Type
data AppF a =
    GetEnv (Env -> a)
  | Login String BasicAuth LoginInput ( Creds -> a)
  | GetLowerDateBound (Date -> a)
  | GetReservas String Creds (Reservas -> a)
  | GetClases String Creds (Clases -> a)
  | PostReservas String Creds Clases (Array ReservaResult -> a)

derive instance appFFunctor :: Functor AppF

type App :: Type -> Type
type App
  = Free AppF

getEnv :: App Env
getEnv = liftF (GetEnv identity)

getLowerDateBound :: App Date
getLowerDateBound = liftF (GetLowerDateBound identity)

login :: String -> BasicAuth -> LoginInput -> App Creds
login url auth loginInput = liftF (Login url auth loginInput identity)

getReservas :: String -> Creds -> App Reservas
getReservas url creds = liftF (GetReservas url creds identity)

getClases :: String -> Creds -> App Clases
getClases url creds = liftF (GetClases url creds identity)

postReservas :: String -> Creds -> Clases -> App (Array ReservaResult)
postReservas url creds clases = liftF (PostReservas url creds clases identity)

appI :: AppF ~> Aff
appI = case _ of
  (GetEnv next) -> do
    env <- liftEffect Env.getEnv
    pure $ next env
  (Login url auth input next) -> do
    creds <- Request.fetch $ Resources.login url auth input
    pure $ next creds
  (GetLowerDateBound next) -> do
    d <- liftEffect tomorrow
    pure $ next d
  (GetReservas url creds next) -> do
    reservas <- Request.fetch $ Resources.misReservas url creds
    pure $ next reservas
  (GetClases url creds next) -> do
    clases <- Request.fetch $ Resources.clases url creds
    pure $ next clases
  (PostReservas url creds clases next) -> do
    results <- sequential $ for (unwrap clases) (parallel <<< Request.fetch <<< Resources.reserva url creds)
    pure $ next results

    
app :: App (Array ReservaResult)
app = do
  { baseUrls, authInfo, disciplinaId } <- getEnv
  creds <- login baseUrls.usersUrl authInfo.basicAuth authInfo.loginInput
  lowerBound <- getLowerDateBound
  reservas <- getReservas baseUrls.apiUrl creds
  clases <- getClases baseUrls.apiUrl creds
  let
    targetClases = Utils.process disciplinaId lowerBound reservas clases
  results <- postReservas baseUrls.apiUrl creds targetClases
  pure results