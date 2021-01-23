module App.Data.Date (CustomDateTime, parseDateTime, day) where

import Prelude
import Data.Bifunctor (bimap)
import Data.Date as D
import Data.DateTime (DateTime, Day)
import Data.DateTime as DT
import Data.Either (Either, either)
import Data.Formatter.DateTime as F
import Effect.Exception (Error, error)

dateTimeFormat :: String
dateTimeFormat = "YYYY-MM-DD HH:mm:ss.S"

newtype CustomDateTime
  = CustomDateTime DateTime

instance showParsedDateTime :: Show CustomDateTime where
  show (CustomDateTime dt) = either identity identity (F.formatDateTime dateTimeFormat dt)

parseDateTime :: String -> Either Error CustomDateTime
parseDateTime = bimap error CustomDateTime <<< F.unformatDateTime dateTimeFormat

day :: CustomDateTime -> Day
day (CustomDateTime dt) = D.day $ DT.date dt
