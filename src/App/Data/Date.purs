module App.Data.Date (CustomDateTime, parseDateTime, now, toDate) where

import Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError(..), decodeJson)
import Data.Bifunctor (bimap, lmap)
import Data.DateTime (Date, DateTime, date)
import Data.Either (Either, either)
import Data.Formatter.DateTime as F
import Data.JSDate as JSDate
import Data.Maybe (fromJust)
import Effect (Effect)
import Effect.Exception (Error, error)
import Partial.Unsafe (unsafePartial)

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

instance decodeJsonCustomDateTime :: DecodeJson CustomDateTime where
  decodeJson json = do
    let
      parseDateTime' = lmap (TypeMismatch <<< show) <<< parseDateTime
    unparsed <- decodeJson json
    parsed <- parseDateTime' unparsed
    pure parsed

parseDateTime :: String -> Either Error CustomDateTime
parseDateTime = bimap error CustomDateTime <<< F.unformatDateTime dateTimeFormat

now :: Effect CustomDateTime
now = do
  jsdate <- JSDate.now
  pure $ CustomDateTime $ unsafePartial $ fromJust $ JSDate.toDateTime jsdate

toDate :: CustomDateTime -> Date
toDate (CustomDateTime dt) = date dt

