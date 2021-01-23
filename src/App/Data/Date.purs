module App.Data.Date (CustomDateTime, parseDateTime) where

import Prelude
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.Either (Either, either)
import Data.Formatter.DateTime as F
import Effect.Exception (Error, error)

dateTimeFormat :: String
dateTimeFormat = "YYYY-MM-DD HH:mm:ss.S"

newtype CustomDateTime
  = CustomDateTime DateTime

instance showCustomDateTime :: Show CustomDateTime where
  show (CustomDateTime dt) = either identity identity (F.formatDateTime dateTimeFormat dt)

instance eqCustomDateTime :: Eq CustomDateTime where
  eq (CustomDateTime a) (CustomDateTime b) = eq a b

instance ordCustomDateTime :: Ord CustomDateTime where
  compare (CustomDateTime a) (CustomDateTime b) = compare a b

parseDateTime :: String -> Either Error CustomDateTime
parseDateTime = bimap error CustomDateTime <<< F.unformatDateTime dateTimeFormat
