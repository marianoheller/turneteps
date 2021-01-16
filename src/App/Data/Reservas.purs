module App.Data.Reservas where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.:))

type ReservaData
  = { fechaHora :: String
    , disciplina :: String
    , coach :: String
    , posicion :: String
    , clubId :: String
    , claseId :: Int
    }

newtype Reservas
  = Reservas (Array ReservaData)

instance showReservas :: Show Reservas where
  show (Reservas reservaData) = show reservaData

instance decodeJsonReservas :: DecodeJson Reservas where
  decodeJson json = do
    obj <- decodeJson json
    result <- obj .: "result"
    pure $ Reservas result

{- 

{
  "code":0,
  "message":"OK",
  "result":[
    {
      "fechaHora":"2021-01-04 07:00:00.0",
      "disciplina":"1. Musculación + Cardio",
      "coach":"Staff Megatlon",
      "posicion":"0",
      "clubId":"36",
      "claseId":15331
    }
  ]
}

 -}
