module App.Date (ParsedDateTime, ParsedDate, parseDateTime, parseDate, getYesterday, parseDateString) where

import Prelude
import Control.Bind (bindFlipped)
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Formatter.DateTime as F
import Effect (Effect)
import Effect.Now (nowDateTime)

dateTimeFormat :: String
dateTimeFormat = "YYYY-MM-DDThh:mm"

dateFormat :: String
dateFormat = "YYYY-MM-DD"

newtype ParsedDateTime
  = MkParsedDateTime String

newtype ParsedDate
  = MkParsedDate String

instance showParsedDateTime :: Show ParsedDateTime where
  show (MkParsedDateTime formattedStr) = formattedStr

instance showParsedDate :: Show ParsedDate where
  show (MkParsedDate formattedStr) = formattedStr

parseDateTime :: DateTime -> Either String ParsedDateTime
parseDateTime = (map MkParsedDateTime) <<< F.formatDateTime dateTimeFormat

parseDate :: DateTime -> Either String ParsedDate
parseDate = (map MkParsedDate) <<< F.formatDateTime dateFormat

parseDateString :: String -> Either String ParsedDate
parseDateString = (bindFlipped parseDate) <<< F.unformatDateTime dateFormat

getYesterday :: Effect DateTime
getYesterday = nowDateTime -- TODO
