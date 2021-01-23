module App.BL (getTargetClases) where

import App.Data.Clases (Clases)
import App.Data.Date (CustomDateTime)
import App.Data.Reservas (Reservas)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))

getTargetClases :: CustomDateTime -> CustomDateTime -> Map CustomDateTime Reservas -> Map CustomDateTime Clases -> Map CustomDateTime Clases
getTargetClases lower upper reservas clases = M.foldSubmap (Just lower) (Just upper) (\k v -> M.singleton k v) clases
