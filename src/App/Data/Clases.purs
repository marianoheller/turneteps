module App.Data.Clases (Clases(..), Clase(..), ClaseData, groupPerDate) where

import Prelude
import App.Data.Date (CustomDateTime, toDate)
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Date (Date)
import Data.Map (Map)
import Data.Map as M
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))

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
  show (Clase clase) = show clase

newtype Clases
  = Clases (Array Clase)

instance showClases :: Show Clases where
  show (Clases clases) = show clases

instance semigroupClases :: Semigroup Clases where
  append (Clases a) (Clases b) = Clases (a <> b)

derive instance newTypeClases :: Newtype Clases _

instance decodeJsonClase :: DecodeJson Clase where
  decodeJson json = do
    obj <- decodeJson json
    claseId <- obj .: "claseId"
    coachId <- obj .: "coachId"
    disciplinaId <- obj .: "disciplinaId"
    disponibilidad <- obj .: "disponibilidad"
    duracion <- obj .: "duracion"
    fecha <- obj .: "fecha"
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

groupPerDate :: Clases -> Map Date Clases
groupPerDate (Clases clases) =
  let
    mapper c@(Clase d) = Tuple (toDate d.fecha) (singleton c)
  in
    M.fromFoldableWith (<>) $ map mapper clases
