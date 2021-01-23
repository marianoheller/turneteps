module App.Data.Clases (Clases(..), Clase(..), ClaseData, singleton) where

import Prelude
import App.Data.Date (CustomDateTime, parseDateTime)
import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Bifunctor (lmap)
import Data.Traversable (traverse)

type ClaseData
  = { claseId :: Int
    , coachId :: Int
    , disciplinaId :: Int
    , disponibilidad :: String
    , duracion :: String
    , fecha :: CustomDateTime
    , reservas :: Int
    }

newtype Clase
  = Clase ClaseData

instance showClase :: Show Clase where
  show (Clase clasesData) = show clasesData

newtype Clases
  = Clases (Array Clase)

instance semigroupClases :: Semigroup Clases where
  append (Clases a) (Clases b) = Clases (a <> b)

instance decodeJsonClase :: DecodeJson Clase where
  decodeJson json = do
    let
      parseDateTime' = lmap (TypeMismatch <<< show) <<< parseDateTime
    obj <- decodeJson json
    claseId <- obj .: "claseId"
    coachId <- obj .: "coachId"
    disciplinaId <- obj .: "disciplinaId"
    disponibilidad <- obj .: "disponibilidad"
    duracion <- obj .: "duracion"
    unparsedFecha <- obj .: "fecha"
    fecha <- parseDateTime' unparsedFecha
    reservas <- obj .: "reservas"
    pure $ Clase { claseId, coachId, disciplinaId, disponibilidad, duracion, fecha, reservas }

instance decodeJsonReservas :: DecodeJson Clases where
  decodeJson json = do
    obj <- decodeJson json
    result <- obj .: "result"
    arrClases <- traverse decodeJson result
    pure $ Clases arrClases

singleton :: Clase -> Clases
singleton c = Clases [ c ]

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
