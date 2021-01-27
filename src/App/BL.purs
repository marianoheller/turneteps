module App.BL where

import Prelude
import App.Data.Clases (Clase(..), Clases(..))
import App.Data.Clases as Clases
import App.Data.Reservas (Reservas)
import App.Data.Reservas as Reservas
import Data.Array (snoc, sortBy)
import Data.Date (Date)
import Data.Foldable (find, foldl)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Foreign.Date (addDaysDate)

filterDays :: Date -> Map Date Reservas -> Map Date Clases -> Map Date Clases
filterDays lower reservas clases =
  let
    upper = addDaysDate 2 lower

    filteredClases = M.submap (Just lower) (Just upper) clases
  in
    M.difference filteredClases reservas

filterClases :: Clases -> Maybe Clase
filterClases (Clases clases) =
  let
    compareDate (Clase a) (Clase b) = compare a.fecha b.fecha
  in
    find (\(Clase clase) -> clase.coachId == 3) $ sortBy (compareDate) clases

process :: Int -> Date -> Reservas -> Clases -> Clases
process targetDisciplinaId lower reservas clases =
  let
    mapReservas = Reservas.groupPerDate reservas

    mapClases = Clases.groupPerDate clases

    folder acc mclases = maybe acc (snoc acc) (filterClases mclases)
  in
    wrap $ foldl folder [] $ M.values $ filterDays lower mapReservas mapClases
