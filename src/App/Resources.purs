module App.Resources where

import Prelude
import Data.Newtype (wrap)
import Milkis as Milkis
import Option as Option

baseUrl :: String
baseUrl = "https://api.turni.to/api"

type ClaseId
  = String

type Slot
  = { start :: String, end :: String, status :: String }

type EndpointBase
  = ( method :: Milkis.Method, url :: Milkis.URL )

type EndpointExtras
  = ( body :: String )

type Endpoint
  = Option.Record EndpointBase EndpointExtras

misTurnos :: String -> Endpoint
misTurnos dateFrom =
  Option.recordFromRecord
    { method: Milkis.getMethod
    , url: wrap $ baseUrl <> "/user/reservation?max=10&offset=0&dateFrom=" <> dateFrom
    }

profile :: Endpoint
profile =
  Option.recordFromRecord
    { method: Milkis.getMethod
    , url: wrap $ baseUrl <> "/user/person"
    }

fechas :: ClaseId -> Endpoint
fechas claseId =
  Option.recordFromRecord
    { method: Milkis.getMethod
    , url: wrap $ baseUrl <> "/user/megatlon/service/" <> claseId
    }

turnos :: ClaseId -> String -> Endpoint
turnos claseId formattedDate =
  Option.recordFromRecord
    { method: Milkis.getMethod
    , url: wrap $ baseUrl <> "/user/megatlon/service/" <> claseId <> "/slots/" <> formattedDate
    }

reserva :: Slot -> ClaseId -> Endpoint
reserva { start, end } claseId =
  Option.recordFromRecord
    { method: Milkis.postMethod
    , url: wrap $ baseUrl <> "/user/megatlon/service/" <> claseId <> "/reservation"
    , body: show { start, end }
    }
