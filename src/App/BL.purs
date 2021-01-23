module App.BL where

import Prelude
import App.Data.Clases (Clase(..), Clases(..))
import App.Data.Clases as C
import App.Data.Date (day)
import Data.Date (Day)
import Data.Map (Map)
import Data.Map as M
import Data.Tuple (Tuple(..))

{- 


descripcion: "1. Musculación + Cardio"
id: 3
imagen: "labujtabn.jpg"
nombre: "1. Musculación + Cardio"
tipo: null

 -}

groupPerDay :: Clases -> Map Day Clases
groupPerDay (Clases clases) =
  let
    mapper c@(Clase d) = Tuple (day d.fecha) (C.singleton c)
  in
    M.fromFoldableWith (<>) $ map mapper clases
