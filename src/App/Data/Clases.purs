module App.Data.Clases (Clases) where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.:))

type ClaseData
  = { claseId :: Int
    , coachId :: Int
    , disciplinaId :: Int
    , disponibilidad :: String
    , duracion :: String
    , fecha :: String
    , reservas :: Int
    }

newtype Clases
  = Clases (Array ClaseData)

instance showReservas :: Show Clases where
  show (Clases clasesData) = show clasesData

-- TODO: improve decoding
instance decodeJsonReservas :: DecodeJson Clases where
  decodeJson json = do
    obj <- decodeJson json
    result <- obj .: "result"
    pure $ Clases result

{- 

{
  "code":0,
  "message":"OK",
  "result":[
    {
      claseId: 757814
      coachId: 3
      disciplinaId: 16
      disponibilidad: "11"
      duracion: "60"
      fecha: "2021-01-16 09:00:00.0"
      reservas: 14
    }
  ]
}

 -}
