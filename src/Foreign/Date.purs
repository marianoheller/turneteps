module Foreign.Date where

import Prelude
import Data.Date (Date)
import Data.DateTime (DateTime(..), adjust, date)
import Data.Enum (toEnum)
import Data.JSDate (JSDate, fromDateTime, now, toDate, toDateTime)
import Data.Maybe (Maybe, fromJust)
import Data.Time (Time(..))
import Data.Time.Duration (Hours(..), negateDuration)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

foreign import subDays :: Int -> JSDate -> JSDate

foreign import addDays :: Int -> JSDate -> JSDate

_unsafeFromJust :: forall a. Maybe a -> a
_unsafeFromJust = unsafePartial $ fromJust

dateToDateTime :: Date -> DateTime
dateToDateTime d =
  let
    t = Time (unsafePartial $ fromJust $ toEnum 0) (unsafePartial $ fromJust $ toEnum 0) (unsafePartial $ fromJust $ toEnum 0) (unsafePartial $ fromJust $ toEnum 0)
  in
    DateTime d t

addDaysDate :: Int -> Date -> Date
addDaysDate n d = (_unsafeFromJust <<< toDate) $ addDays n (fromDateTime (dateToDateTime d))

today :: Effect Date
today = do
  jsdt <- now
  let
    timezoneOffset = negateDuration (Hours 3.0)

    dt' =
      _unsafeFromJust do
        dt <- toDateTime jsdt
        adjust timezoneOffset dt
  pure $ date dt'

tomorrow :: Effect Date
tomorrow = addDaysDate 1 <$> today
