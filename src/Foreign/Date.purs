module Foreign.Date where

import Prelude
import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.JSDate (JSDate, fromDateTime, now, toDate)
import Data.Maybe (fromJust)
import Data.Time (Time(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

foreign import subDays :: Int -> JSDate -> JSDate

foreign import addDays :: Int -> JSDate -> JSDate

_unsafeToDate :: JSDate -> Date
_unsafeToDate = unsafePartial $ fromJust <<< toDate

dateToDateTime :: Date -> DateTime
dateToDateTime d =
  let
    t = Time (unsafePartial $ fromJust $ toEnum 0) (unsafePartial $ fromJust $ toEnum 0) (unsafePartial $ fromJust $ toEnum 0) (unsafePartial $ fromJust $ toEnum 0)
  in
    DateTime d t

addDaysDate :: Int -> Date -> Date
addDaysDate n d = _unsafeToDate $ addDays n (fromDateTime (dateToDateTime d))

today :: Effect Date
today = do
  jsdt <- now
  pure $ _unsafeToDate jsdt
