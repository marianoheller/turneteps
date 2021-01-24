module App.Data.Categorias (Categorias(..), Categoria(..), CategoriaData) where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.:))
import Data.Traversable (traverse)

type CategoriaData
  = { id :: Int
    , nombre :: String
    , descripcion :: String
    }

newtype Categoria
  = Categoria CategoriaData

instance showClase :: Show Categoria where
  show (Categoria clase) = show clase

newtype Categorias
  = Categorias (Array Categoria)

instance showClases :: Show Categorias where
  show (Categorias clases) = show clases

instance decodeJsonClase :: DecodeJson Categoria where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .: "id"
    nombre <- obj .: "nombre"
    descripcion <- obj .: "descripcion"
    pure $ Categoria { id, nombre, descripcion }

instance decodeJsonReservas :: DecodeJson Categorias where
  decodeJson json = do
    obj <- decodeJson json
    result <- obj .: "result"
    arrCats <- traverse decodeJson result
    pure $ Categorias arrCats



{- 
{
  "code": 0,
  "message": "OK",
  "result": [
    {
      "id": 3,
      "nombre": "1. Musculación + Cardio",
      "descripcion": "1. Musculación + Cardio",
      "tipo": null,
      "imagen": "labujtabn.jpg"
    },
    {
      "id": 11,
      "nombre": "2. Spinning Indoor",
      "descripcion": "2. Spinning Indoor",
      "tipo": null,
      "imagen": "kcfyqlfkud.jpg"
    }
  ]
}
 -}
