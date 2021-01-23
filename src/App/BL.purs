module App.BL (getTargetClases) where

import App.Data.Clases (Clases)
import App.Data.Reservas (Reservas)
import Data.Date (Date)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Foreign.Date (addDaysDate)

getTargetClases :: Date -> Map Date Reservas -> Map Date Clases -> Map Date Clases
getTargetClases lower reservas clases =
  let
    upper = addDaysDate 3 lower

    filteredClases = M.submap (Just lower) (Just upper) clases
  in
    M.difference filteredClases reservas
