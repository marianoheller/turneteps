module App.Data.Reservas (Reservas(..), Reserva(..), ReservaData, groupPerDateTime) where

import Prelude
import App.Data.Date (CustomDateTime)
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Map (Map)
import Data.Map as M
import Data.Tuple (Tuple(..))

type ReservaData
  = { fechaHora :: CustomDateTime
    , disciplina :: String
    , coach :: String
    , posicion :: String
    , clubId :: String
    , claseId :: Int
    }

newtype Reserva
  = Reserva ReservaData

instance showReserva :: Show Reserva where
  show (Reserva reserva) = show reserva

newtype Reservas
  = Reservas (Array Reserva)

instance showReservas :: Show Reservas where
  show (Reservas reservas) = show reservas

instance semigroupReservas :: Semigroup Reservas where
  append (Reservas a) (Reservas b) = Reservas (a <> b)

instance decodeJsonReserva :: DecodeJson Reserva where
  decodeJson json = do
    obj <- decodeJson json
    fechaHora <- obj .: "fechaHora"
    disciplina <- obj .: "disciplina"
    coach <- obj .: "coach"
    posicion <- obj .: "posicion"
    clubId <- obj .: "clubId"
    claseId <- obj .: "claseId"
    pure $ Reserva { fechaHora, disciplina, coach, posicion, clubId, claseId }

instance decodeJsonReservas :: DecodeJson Reservas where
  decodeJson json = do
    obj <- decodeJson json
    result <- obj .: "result"
    pure $ Reservas result

singleton :: Reserva -> Reservas
singleton r = Reservas [ r ]

groupPerDateTime :: Reservas -> Map CustomDateTime Reservas
groupPerDateTime (Reservas reservas) =
  let
    mapper c@(Reserva r) = Tuple r.fechaHora (singleton c)
  in
    M.fromFoldableWith (<>) $ map mapper reservas

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
