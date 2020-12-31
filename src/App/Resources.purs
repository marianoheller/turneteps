module App.Resources where

import Prelude

import App.Date (ParsedDate)
import App.Env as Env
import Data.Newtype (wrap)
import Effect.Unsafe (unsafePerformEffect)
import Milkis as M
import Option as Option

type ClaseId
  = String

type Slot
  = { start :: String, end :: String, status :: String }

type ResourceBase
  = ( method :: M.Method, url :: M.URL )

type ResourceExtras
  = ( body :: String )

newtype Resource a
  = Resource (Option.Record ResourceBase ResourceExtras)

type Turnos  -- TODO
  = { requestId :: String
    , exceptionKey :: String
    }

baseUrl :: String
baseUrl = unsafePerformEffect $ Env.getBaseUrl

misTurnos :: ParsedDate -> Resource Turnos
misTurnos dateFrom =
  Resource
    $ Option.recordFromRecord
        { method: M.getMethod
        , url: wrap $ baseUrl <> "/user/reservation?max=10&offset=0&dateFrom=" <> (show dateFrom)
        }

profile :: Resource String
profile =
  Resource
    $ Option.recordFromRecord
        { method: M.getMethod
        , url: wrap $ baseUrl <> "/user/person"
        }

fechas :: ClaseId -> Resource String
fechas claseId =
  Resource
    $ Option.recordFromRecord
        { method: M.getMethod
        , url: wrap $ baseUrl <> "/user/megatlon/service/" <> claseId
        }

turnos :: ClaseId -> String -> Resource String
turnos claseId formattedDate =
  Resource
    $ Option.recordFromRecord
        { method: M.getMethod
        , url: wrap $ baseUrl <> "/user/megatlon/service/" <> claseId <> "/slots/" <> formattedDate
        }

reserva :: Slot -> ClaseId -> Resource String
reserva { start, end } claseId =
  Resource
    $ Option.recordFromRecord
        { method: M.postMethod
        , url: wrap $ baseUrl <> "/user/megatlon/service/" <> claseId <> "/reservation"
        , body: show { start, end }
        }
